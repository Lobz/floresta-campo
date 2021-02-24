/***
* Name: araucariaandfire
* Author: oz
* Description: 
* Tags: Tag1, Tag2, TagN
***/

model instafire

global {
	float step <- 1#y;
	// spatial scale vars
	float landscape_size <- 600#m;
	float tile_size <- 10#m;
	
	// derived spatial scale vars
	geometry shape <- square(landscape_size);
	int size <- round(landscape_size/tile_size);
	float tile_area <- tile_size^2;
	point center <- {landscape_size/2, landscape_size/2};
	
	// toggle phenomena
	bool wildfires <- true;
	string topography <- "plain";
	
	// general parameters
	float chance_to_start_fire <- 0.1;
	
	// Initial forest parameters
	int initial_pop_total <- 200;
	float initial_pop_ratio <- 0.5;
	int initial_pop_araucaria <- floor(initial_pop_total*initial_pop_ratio);
	int initial_pop_broadleaf <- initial_pop_total - initial_pop_araucaria;
	float initial_forest_size <- 50#m;
	int initial_tree_stage <- 4;
	
	// Grass parameters
	float grass_growthrate <- 0.5;
	float grass_flamability <- 0.9;
	float biomass_loss_burning <- 0.8;
	float minimum_biomass <- 0.1;
	// float carrying_capacity <- 1.0; // normalized
	
	// Tree parameters
	
	// values taken from Paludo et al 2016
	list<float> tree_reproduction_rate1 <- [0,0,0,0,0.688]; // produce stage 1 seedlings
	list<float> tree_reproduction_rate2 <- [0,0,0,0,0.071]; // produce stage 2 seedlings
	list<float> tree_survival_rate <- [0.008,0.792,0.927,0.970,0.997];
	list<float> tree_lit_growth_rate <- [0.197,0.013,0.009,0.008,0];
	// calculated from lit values
	list<float> tree_death_rate <-[0.795, 0.195, 0.064, 0.022, 0.003]; // d=1-s-g
	list<float> tree_ind_lit_growth_rate <- [0.96097561, 0.016149068, 0.009615385, 0.008179959, 0.0]; // (1-d)g*=g ===> g* = g/(g+s), so we can have independant coin flips for death and growth
	
	// derived values
	list<float> tree_growth_rate <- [1.0,0.1,0.1,0.1,0];//ind_lit_growth_rate collect (each*2 > 1 ? 1 : each*2);
	list<float> tree_canopy_size <- [0.1,0.5,1,2,5];
	list<float> tree_height <- [0.1,0.5,1.0,10,40];
	
	// divergent values
	float araucaria_base_flammability <- 0.7 parameter:true;
	float bl_s3_fla <- 1.0 parameter:true;
	list<float> araucaria_flammability <- [1.0,1.0,araucaria_base_flammability,araucaria_base_flammability^2,araucaria_base_flammability^4];
	list<float> broadleaf_flammability <- [1.0,1.0,bl_s3_fla,bl_s3_fla^2,bl_s3_fla^4];
	float shade_threshold_araucaria <- 1.0;
	float shade_threshold_ratio <- 3.0;
	float shade_threshold_broadleaf <- shade_threshold_araucaria*shade_threshold_ratio;
	float araucaria_dispersal <- 10#m;
	float broadleaf_dispersal <- 10#m;

	
	// Monitoring
	int nb_araucaria -> {length(araucaria where (each.stage > 3))};
	int nb_broadleaf -> {length(broadleaf where (each.stage > 3))};
	int firesize -> {first(wildfire).firesize};
	point forest_radius (list<tree> valid_trees) {
		if(empty(valid_trees)) {
			return {0,0};
		}
		int n95 <- round(floor(length(valid_trees)*0.95));
		int n05 <- round(floor(length(valid_trees)*0.05));
		list<float> sq_dists <- valid_trees accumulate ((each.location.x-center.x)^2 + (each.location.y-center.y)^2);
		sq_dists <- sq_dists sort each;
		
		return {sqrt(sq_dists[n05]),sqrt(sq_dists[n95])};
	}
	
	point rad_h -> forest_radius(araucaria where (each.stage = 4));
	point rad_u -> forest_radius(broadleaf where (each.stage = 4));
	
	init {		
		geometry c <- circle(initial_forest_size);
		create broadleaf number: initial_pop_broadleaf{
			stage <- initial_tree_stage;
			location <- any_location_in(c);
			do real_init;
		}
		create araucaria number: initial_pop_araucaria{
			stage <- initial_tree_stage;
			location <- any_location_in(c);
			do real_init;
		}
		
		
	}
		

}

species scheduler schedules: wildfire + shuffle(grass) + shuffle(broadleaf + araucaria); // explicit scheduling in the world

grid wildfire width:1 height:1 schedules: []{
	int firesize <- 0 update: 0;
	
	reflex start_fire when: wildfires and flip(chance_to_start_fire) {
		list<grass> potential <- grass where (each.biomass > 0.9);
		if(empty(potential)){
			potential <- grass where (each.biomass > 0.0);
		}
		grass random_tile <- first (shuffle(potential));
		do spread_fire(random_tile);
	}

	
	action spread_fire (grass starter) {
		list<grass> burning <- [starter]; // burning
		list<grass> burnt <- []; // done
		
		loop while: not(empty(burning)) {
			burnt <- burnt union burning; // finished burning
			burning <- remove_duplicates(burning accumulate each.spread_fire(burnt)); // spread to neighbors of burning, ignoring the burnt
		}
		ask burnt {do burn();} // burn everyone
		firesize <- length(burnt);
	}	
}

grid grass height:size width:size neighbors: 4  schedules: []{
	
	float biomass <- minimum_biomass; // this value is normalized to be in range (0,1]
	float carrying_capacity <- 1.0; //dependant on shade
	float flamability <- grass_flamability*biomass update: grass_flamability*biomass;
	float growthrate <- grass_growthrate;
	int last_burned <- 20 update: last_burned +1;
	float my_height <- 0.0;
	
	float altitude;
	
	init {
		float l <- landscape_size;
		switch topography {
			match "plain"  {altitude <-0.0; }
			match "valley" {altitude <- ((location.x - l/2)/10)^2 + ((location.y - l/2)/20)^2;}
			match "ridge"  {altitude <- l-(((location.x - l/2)/10)^2 + ((location.y - l/2)/20)^2);}
		}
	}
	
	list<tree> here;
	
	float shade_over_height (float height) {
		list<tree> trees <- self.here where (each.my_height > height);
		float shade_per_meter <- sum(trees accumulate each.my_canopy_area) / tile_area;
		return shade_per_meter;
	}
	
	reflex shade {
		carrying_capacity <- 1.0/(self.shade_over_height(0.0)+1);
	}
		
	reflex plant_growth{
		if(biomass <= minimum_biomass) {biomass<-minimum_biomass;}
		biomass <- biomass * (1+growthrate * (1 - biomass/carrying_capacity));
	}
	
	action burn {
		biomass <- biomass*biomass_loss_burning;
		last_burned <- 0;
		loop t over: here {ask t {do burn;}}
	}
	
	float spread_chance(grass a,grass b) {
		float distab <- abs(a.location.x-b.location.x) + abs(a.location.y - b.location.y);
		float slope <- (b.altitude - a.altitude)/(distab);
		float slopeeffect;
		
		// I want a function that is equal do 1 when slope is 0.4 (aprox 20 degrees)
		if(slope >= 0.4) {
			slopeeffect <- 1.0;
		}
		else if(slope <= -0.4) {
			slopeeffect <- 0.2;
		}
		else {
			slopeeffect <- 1.0*(slope+0.6);
		}
		
		return b.flamability*slopeeffect;
	}
	
	list<grass> spread_fire (list<grass> remove) {
		return (self.neighbors - remove) where flip(spread_chance(self, each));
	}
	
	// this will be used by default "grid" display
	rgb color<- rgb(255*(biomass),255*(biomass),0) 
		update: last_burned <1 ? #red : rgb(255*(biomass),255*(biomass),0,0.5);
		
	aspect trid {
		draw shape color: color depth: altitude;
	}
}

species tree schedules: [] {
	grass place;
	list<grass> places;
	list<tree> neighbors;
	float shade_ratio <- 1.0;
	int stage <- 0;
	float shade_effect <- 0.0;
	float n_shade <-0.0;
	float my_dispersal;
	float my_shade_threshold;
	rgb my_color;
	int my_first_cycle <- -1;
	
	list<float> flammability;
	
	float my_reproduction_rate1;
	float my_reproduction_rate2;
	float my_death_rate;
	float my_growth_rate;
	float my_flamability;
	float my_canopy_size;
	float my_height;
	float my_canopy_area;
	geometry my_canopy;
	geometry shape <- circle(0.5); // radius of firecatching?
	
	// methods
	action update_traits {
		self.my_reproduction_rate1 <- tree_reproduction_rate1[self.stage];
		self.my_reproduction_rate2 <- tree_reproduction_rate2[self.stage];
		self.my_death_rate <- tree_death_rate[self.stage];
		self.my_growth_rate <- tree_growth_rate[self.stage] * self.shade_ratio;
		self.my_canopy_size <- tree_canopy_size[self.stage];
		self.my_height <- tree_height[self.stage];
		
		// divergente values
		self.my_flamability <- self.flammability[self.stage];
		
		self.my_canopy_area <- 3.14*self.my_canopy_size^2;
		self.shape <- circle(my_canopy_size);
		self.places <- grass overlapping shape;
	}
	
	action real_init {
		place <- first (grass overlapping self);
		if(place=nil) {do die;}
		place.here <- place.here + self;
		do update_traits();
		do get_shade();
	}
	
	action get_shade {
		if (place.shade_over_height(my_height) > my_shade_threshold) {
			self.shade_ratio <- shade_effect;
		}
		do update_traits();
	}
	
	action burn {
		if(flip(my_flamability)){
			place.here <- place.here - self;
			do die;
		}
	}
	
	point disperse {
		point starting <- self.location;
		float gamma_scale <- self.my_dispersal;
		float gamma_shape <- 1.0;
		float distance <- gamma_rnd(gamma_shape, gamma_scale);
		float angle <- rnd(360.0);
		point vector <- {cos(angle)*distance,sin(angle)*distance};
		return self.location + vector;
	}
	
	// reflexes
	reflex shade {
		do get_shade();
	}
	
	reflex natural_death when: flip(my_death_rate){
		place.here <- place.here - self;
		do die;
	}
	
	reflex grow when: flip(my_growth_rate){
		stage <- stage + 1;
		do update_traits();
	}
	
	reflex reproduce when: flip(my_reproduction_rate1){
		create species_of(self) {
			location <- myself.disperse();
			my_first_cycle <- cycle;
			do real_init;
		}
	}
	
	reflex reproduce2 when: flip(my_reproduction_rate2){
		create species_of(self) {
			location <- myself.disperse();
			stage <- 1;
			my_first_cycle <- cycle;
			do real_init;
		}
	}	
	
	// graphics
	aspect default {
		draw shape color: my_color border:#black;
	}
}

species araucaria parent:tree schedules: []{
	list<float> flammability <- araucaria_flammability;
	float my_shade_threshold <- shade_threshold_araucaria;
	float my_dispersal <- araucaria_dispersal;
	rgb my_color <- rgb(0,255,0,0.5);
}

species broadleaf parent:tree schedules: []{
	list<float> flammability <- broadleaf_flammability;
	float my_shade_threshold <- shade_threshold_broadleaf;
	float my_dispersal <- broadleaf_dispersal;
	rgb my_color <- rgb(245,0,0,0.5);
	
}


experiment instafire type: gui {

	
	// Parameters
	parameter "Landscape size" category: "Init" var: landscape_size min:0.0;
	parameter "Patch size" category: "Init" var: initial_forest_size min:0.0;
	parameter "Tile size" category: "Init" var: tile_size min:1#m;
	parameter "Initial light demanding pop" category: "Init" var: initial_pop_araucaria min:0;
	parameter "Initial shade tolerant pop" category: "Init" var: initial_pop_broadleaf min:0;
	parameter "Initial forest size" category: "Init" var: initial_forest_size min:0.0;
	parameter "Average araucaria dispersal" category: "Init" var: araucaria_dispersal min:0.0;
	parameter "Average broadleaf dispersal" category: "Init" var: broadleaf_dispersal min:0.0;
	parameter "Araucaria shade tolerance" category: "Init" var: shade_threshold_araucaria min:0.0;
	parameter "Broadleaf shade tolerance" category: "Init" var: shade_threshold_broadleaf min:0.0;
	parameter "Topography" category:"Init" var: topography among: ["plain","valley","ridge"];
	
	parameter "Wildfires" category: "Fire" var:wildfires;
	
	parameter "Grass growth rate" category: "Grass" var: grass_growthrate min:0.001 max:0.5;
	parameter "Chance of fire" category: "Fire" var: chance_to_start_fire min:0.0;
	parameter "Grass flamability" category: "Grass" var: grass_flamability min:0.0;
	
	// Define attributes, actions, a init section and behaviors if necessary
	
	
	
	output {
		display "Number of trees" {
			chart "Number of adult trees" type: series size: {1,0.5} position: {0, 0} {
        		data "Number of araucaria trees" value: nb_araucaria color: #darkgreen ;
        		data "Number of broadleaf trees" value: nb_broadleaf color: #red ;
        	}
        }
        
		display "Radius of circle with 95% of adult trees" {
			chart "Distance from center of adult trees" type: series size: {1,0.5} position: {0, 0} {
        		data "araucaria trees - outer" value: rad_h.y color: #darkgreen ;
        		data "broadleaved trees - outer" value: rad_u.y color: #red ;
        		data "araucaria trees - inner" value: rad_h.x color: #darkgreen ;
        		data "broadleaved trees - inner" value: rad_u.x color: #red ;
        	}
        }
        
        display "Wildfires" {
			chart "Size of the last fire" type: series style: stack size: {1,0.5} position: {0, 0} y_range: {0,size^2} {
        		data "Number of terrain tiles" value:firesize color: #black ;
        	}
        }
	
		display "model" {
			grid grass;
			species broadleaf;
			species araucaria;
			//event mouse_up action: click;
		}
		
		//display "model 3D" camera_interaction:false camera_pos:{world.shape.width/2,world.shape.height*2,world.shape.width*2} 
		//camera_look_pos:{world.shape.width/2,world.shape.height/2,0} 
		//camera_up_vector:{0.0,-1.0,0.0}type:opengl{
		//	species grass aspect: trid;
		//}
	}
}

experiment fireandforest type: gui {
	parameter "Landscape size" category: "Init" var: landscape_size min:0.0;
	parameter "Patch size" category: "Init" var: initial_forest_size min:0.0;
	parameter "Tile size" category: "Init" var: tile_size min:1#m;
	parameter "initial_pop_total" category: "Init" var: initial_pop_total min:0;
	parameter "initial_pop_ratio" category: "Init" var: initial_pop_ratio min:0 max:1.0;
	parameter "Initial forest size" category: "Init" var: initial_forest_size min:0.0;
	parameter "Average araucaria dispersal" category: "Init" var: araucaria_dispersal min:0.0;
	parameter "Average broadleaf dispersal" category: "Init" var: broadleaf_dispersal min:0.0;
	parameter "shade_threshold_araucaria" category: "Init" var: shade_threshold_araucaria min:0.0;
	parameter "Shade tolerance ratio" category: "Init" var: shade_threshold_ratio min:1.0 max:5.0;
	parameter "Topography" category:"Init" var: topography among: ["plain","valley","ridge"];
	
	parameter "Wildfires" category: "Fire" var:wildfires;
	parameter "Chance of fire" category: "Fire" var: chance_to_start_fire min:0.0 max:1.0;
	
	
	parameter "araucaria_base_flammability" var: araucaria_base_flammability min:0.0 max:1.0;
	
	
	output {

    	monitor "Number of araucaria trees" value: nb_araucaria;
    	monitor "Number of broadleaved trees" value: nb_broadleaf;
		monitor "Circle size for araucaria trees" value: rad_h.y;
        monitor "Circle size for broadleaved trees" value: rad_u.y;
		monitor "Size of fire" value:firesize;
		monitor "Chance of fire" value: wildfires? chance_to_start_fire : 0;
		monitor "Initial Araucaria pop"  value: initial_pop_araucaria;
		monitor "Initial broadleaved pop"  value: initial_pop_broadleaf;
		monitor "Araucaria shade tolerance" value: shade_threshold_araucaria;
		monitor "Shade tolerance ratio" value: shade_threshold_ratio;
		monitor "araucaria_base_flammability" value: araucaria_base_flammability;
		
	}
}

experiment explore type: batch repeat: 20 keep_seed: true until: ( time > 1000 ) {
	parameter "Initial forest size" category: "Init" var: initial_forest_size min:50 max:200;
	parameter "Average araucaria dispersal" category: "Init" var: araucaria_dispersal min:5.0 max:50;
	parameter "Average broadleaf dispersal" category: "Init" var: broadleaf_dispersal min:5.0 max:50;
	parameter "Chance of fire" category: "Fire" var: chance_to_start_fire min:0.0 max:1.0;

	parameter "Araucaria shade tolerance" category: "Init" var: shade_threshold_araucaria min:0.5 max:2.5;
	parameter "Shade tolerance ratio" category: "Init" var: shade_threshold_ratio min:2.5 max:4.5;
    
    
    
    reflex save_results_explo {
        ask simulations {
        }        
    }
}