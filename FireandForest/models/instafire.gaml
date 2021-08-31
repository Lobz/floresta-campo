/***
* Name: araucariaandfire
* Author: oz
* Description: 
* Tags: Tag1, Tag2, TagN
***/

model instafire

global {
	// for float approximations
	float epsilon <- 0.0001;
	
	// needed for batch simulation tracking
	float par_group <- 1;
	bool graphic;


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
	
	// general parameters
	float wildfire_rate <- 0.1;
	
	// Initial forest parameters
	int initial_pop_total <- 200;
	float initial_pop_ratio <- 0.5;
	int initial_pop_araucaria <- floor(initial_pop_total*initial_pop_ratio);
	int initial_pop_broadleaf <- initial_pop_total - initial_pop_araucaria;
	float initial_forest_size <- 50#m;
	int initial_tree_stage <- 4;
	
	// Grass parameters
	float grass_growthrate <- 0.5;
	float grass_flammability <- 0.6;
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
	list<float> tree_height <- [0.1,1,2,10,40];
	float tree_dispersal <- 10#m;
	
	// divergent values
	float araucaria_base_fire_tolerance <- 0.9 parameter:true;
	float araucaria_s5_fire_tolerance <- araucaria_base_fire_tolerance;
	float araucaria_s4_fire_tolerance <- araucaria_s5_fire_tolerance^2;
	float araucaria_s3_fire_tolerance <- araucaria_s5_fire_tolerance^4;
	list<float> araucaria_fire_tolerance <- [0,0,araucaria_s3_fire_tolerance,araucaria_s4_fire_tolerance,araucaria_s5_fire_tolerance];
	list<float> broadleaf_fire_tolerance <- [0,0,0,0,0];
	float shade_threshold_araucaria <- 1.0;
	float shade_threshold_ratio <- 2.0;
	float shade_threshold_broadleaf <- shade_threshold_araucaria*shade_threshold_ratio;
	float araucaria_dispersal <- tree_dispersal;
	float broadleaf_dispersal <- tree_dispersal;

	
	// Monitoring
	int nb_araucaria -> {length(araucaria where (each.stage > 3))};
	int nb_broadleaf -> {length(broadleaf where (each.stage > 3))};
	int firesize -> {first(wildfire).firesize};
	list<float> forest_radius (list<tree> valid_trees, list<float> props) {
		if(empty(valid_trees)) {
			return props accumulate (0);
		}
		
		// calculate distance for each tree
		list<float> sq_dists <- valid_trees accumulate ((each.location.x-center.x)^2 + (each.location.y-center.y)^2);
		sq_dists <- sq_dists sort each;
		
		// turn props into indexes
		list<int> indexes <- props accumulate floor((length(valid_trees)-1)*each); 
		
		return indexes accumulate (sqrt(sq_dists[each]));
	}
	
	list<float> quantiles <- [0.05, 0.50, 0.95];
	list<float> rad_A_adults -> forest_radius(araucaria where (each.stage = 4), quantiles);
	list<float> rad_B_adults -> forest_radius(broadleaf where (each.stage = 4), quantiles);
	//list<float> rad_A_all -> forest_radius(araucaria where (true), quantiles);
	//list<float> rad_B_all -> forest_radius(broadleaf where (true), quantiles);
	
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
	
	float araucaria_growthrate_0 -> (araucaria where (each.stage = 0)) mean_of (each.my_growth_rate);
	float broadleaf_growthrate_0 -> (broadleaf where (each.stage = 0)) mean_of (each.my_growth_rate);
	float araucaria_growthrate_1 -> (araucaria where (each.stage = 1)) mean_of (each.my_growth_rate);
	float broadleaf_growthrate_1 -> (broadleaf where (each.stage = 1)) mean_of (each.my_growth_rate);
	float araucaria_growthrate_2 -> (araucaria where (each.stage = 2)) mean_of (each.my_growth_rate);
	float broadleaf_growthrate_2 -> (broadleaf where (each.stage = 2)) mean_of (each.my_growth_rate);
	float araucaria_growthrate_3 -> (araucaria where (each.stage = 3)) mean_of (each.my_growth_rate);
	float broadleaf_growthrate_3 -> (broadleaf where (each.stage = 3)) mean_of (each.my_growth_rate);


}

species scheduler schedules: wildfire + grass + shuffle(broadleaf + araucaria); // explicit scheduling in the world

grid wildfire width:1 height:1 schedules: [] use_regular_agents: false {
	int firesize <- 0 update: 0;
	
	reflex start_fire when: wildfires and flip(wildfire_rate) {
		list<grass> potential <- grass where (each.biomass > 0.9);
		if(empty(potential)){
			return;
		}
		grass random_tile <- first (shuffle(potential));
		do spread_fire(random_tile);
	}

	
	action spread_fire (grass starter) {
		list<grass> burning <- [starter]; // burning
		list<grass> burnt <- []; // done
		
		loop while: not(empty(burning)) {
			burnt <- burnt union burning; // finished burning
			list<grass> to_burn <- remove_duplicates(burning accumulate (each.spread_fire()) );
			burning <- (to_burn - burnt); // spread to neighbors of burning, ignoring the burnt
		}
		ask burnt {do burn();} // burn everyone
		firesize <- length(burnt);
	}	
}

grid grass height:size width:size neighbors: 4  schedules: [] use_regular_agents: false {
	
	float biomass <- minimum_biomass; // this value is normalized to be in range (0,1]
	float carrying_capacity <- 1.0; //dependant on shade
	float flammability <- grass_flammability*biomass update: grass_flammability*biomass;
	float growthrate <- grass_growthrate;
	float my_height <- 0.0;
	
	float altitude;
	
	list<tree> here;
	
	float shade_over_height (float height) {
		list<tree> trees <- self.here where (each.my_height > height);
		float shade_per_meter <- trees sum_of each.my_canopy_area / tile_area;
		return shade_per_meter;
	}
	
	action shade {
		carrying_capacity <- 1.0/(self.shade_over_height(0.0)+1);
	}
		
	reflex plant_growth when: (abs(carrying_capacity - biomass) > epsilon) {
		biomass <- biomass * (1+growthrate * (1 - biomass/carrying_capacity));
		if(biomass <= minimum_biomass) {biomass<-minimum_biomass;}
	}
	
	action burn {
		biomass <- biomass*biomass_loss_burning;
		ask here {do burn;}
		do shade();
	}
	
	float spread_chance(grass a,grass b) {
		return b.flammability;
	}
	
	list<grass> spread_fire {
		list<grass> to_spread <- self.neighbors where flip(each.flammability);
		return to_spread;
	}
	
	// this will be used by default "grid" display
	rgb color <- rgb(255*(biomass),255*(biomass),0);
	reflex update_color when: graphic {
		color <- (rgb(255*(biomass),255*(biomass),0,0.5));
	}
		
	aspect trid {
		draw shape color: color depth: altitude;
	}
	
	action add_tree (tree t) {
		here <- here + t;
		do update_shade(t.my_height);
	}
	
	action remove_tree (tree t) {
		here <- here - t;
		do update_shade(t.my_height);
	}
	
	action update_shade (float h) {
		do shade();
		ask (here where (each.my_height < h)) {do get_shade;}
	}
}

species tree schedules: [] {
	grass place;
	list<tree> neighbors;
	float shade_ratio <- 1.0;
	int stage <- 0;
	float shade_effect <- 0.0;
	float n_shade <-0.0;
	float my_dispersal;
	float my_shade_threshold;
	rgb my_color;
	int my_first_cycle <- -1;
	
	list<float> fire_tolerance;
	
	float my_reproduction_rate1;
	float my_reproduction_rate2;
	float my_death_rate;
	float my_growth_rate;
	float my_fire_tolerance;
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
		self.my_fire_tolerance <- self.fire_tolerance[self.stage];
		
		self.my_canopy_area <- 3.14*self.my_canopy_size^2;
		if graphic {self.shape <- circle(my_canopy_size);}
		
		ask self.place { do update_shade(my_height); }
	}
	
	action real_init {
		place <- first(grass overlapping self.location);
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
		if(!flip(my_fire_tolerance)){
			ask place { do remove_tree(myself); }
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
	
	reflex natural_death when: flip(my_death_rate){
		ask place { do remove_tree(myself); }
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
		point loc <- disperse();
		if (loc.x < 0 or loc.y < 0 or loc.x > landscape_size or loc.y > landscape_size) {
			return; // dispersed to outside of arena
		}
		create species_of(self) {
			location <- loc;
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
	list<float> fire_tolerance <- araucaria_fire_tolerance;
	float my_shade_threshold <- shade_threshold_araucaria;
	float my_dispersal <- araucaria_dispersal;
	rgb my_color <- rgb(0,200,0,0.5);
}

species broadleaf parent:tree schedules: []{
	list<float> fire_tolerance <- broadleaf_fire_tolerance;
	float my_shade_threshold <- shade_threshold_broadleaf;
	float my_dispersal <- broadleaf_dispersal;
	rgb my_color <- rgb(200,0,150,0.5);
	
}

experiment fireandforest type: gui {
	parameter "landscape_size" category: "Init" var: landscape_size min:0.0;
	parameter "initial_forest_size" category: "Init" var: initial_forest_size min:0.0;
	parameter "tile_size" category: "Init" var: tile_size min:1#m;
	parameter "initial_pop_total" category: "Init" var: initial_pop_total min:0;
	parameter "initial_pop_ratio" category: "Init" var: initial_pop_ratio min:0 max:1.0;
	parameter "tree_dispersal" category: "Init" var: tree_dispersal min:0.0;
	parameter "araucaria_dispersal" category: "Init" var: araucaria_dispersal min:0.0;
	parameter "broadleaf_dispersal" category: "Init" var: broadleaf_dispersal min:0.0;
	parameter "shade_threshold_araucaria" category: "Init" var: shade_threshold_araucaria min:0.0;
	parameter "shade_threshold_ratio" category: "Init" var: shade_threshold_ratio min:1.0 max:5.0;
	
	parameter "Wildfires" category: "Fire" var:wildfires;
	parameter "wildfire_rate" category: "Fire" var: wildfire_rate min:0.0 max:1.0;
	parameter "araucaria_fire_tolerance" var: araucaria_base_fire_tolerance min:0.0 max:1.0;
	parameter "grass_flammability" category: "Grass" var: grass_flammability min:0.0;
	
	parameter "par_group" var: par_group min:1;
	
	
	init {
		graphic <- false;
	}
		
	output {

    	monitor "par_group" value: par_group;
    	monitor "Number of araucaria trees" value: nb_araucaria;
    	monitor "Number of broadleaved trees" value: nb_broadleaf;
		monitor "Size of fire" value:firesize;
		monitor "wildfire_rate" value: wildfires? wildfire_rate : 0;
		monitor "Initial Araucaria pop"  value: initial_pop_araucaria;
		monitor "Initial broadleaved pop"  value: initial_pop_broadleaf;
		monitor "Araucaria shade tolerance" value: shade_threshold_araucaria;
		monitor "Shade tolerance ratio" value: shade_threshold_ratio;
		monitor "araucaria_fire_tolerance" value: araucaria_base_fire_tolerance;
		monitor "araucaria_dispersal" value: araucaria_dispersal;
		monitor "broadleaf_dispersal" value: broadleaf_dispersal;
		monitor "grass_flammability" value: grass_flammability;
		
		monitor "Circle size for araucaria trees" value: rad_A_adults[2];
        monitor "Circle size for broadleaved trees" value: rad_B_adults[2];
		monitor "circle05_araucaria" value: rad_A_adults[0];
        monitor "circle05_broadleaf" value: rad_B_adults[0];
		
		monitor "araucaria_growthrate_0" value: araucaria_growthrate_0;
		monitor "broadleaf_growthrate_0" value: broadleaf_growthrate_0;
		monitor "araucaria_growthrate_1" value: araucaria_growthrate_1;
		monitor "broadleaf_growthrate_1" value: broadleaf_growthrate_1;
		monitor "araucaria_growthrate_2" value: araucaria_growthrate_2;
		monitor "broadleaf_growthrate_2" value: broadleaf_growthrate_2;
		monitor "araucaria_growthrate_3" value: araucaria_growthrate_3;
		monitor "broadleaf_growthrate_3" value: broadleaf_growthrate_3;
		
		
	}
}

experiment fireandforest_graphic type: gui until: time>10  {
	
	init {
		graphic <- true;
		ask (araucaria + broadleaf) {do update_traits;}
	}

	
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
	
	parameter "Wildfires" category: "Fire" var:wildfires;
	
	parameter "Grass growth rate" category: "Grass" var: grass_growthrate min:0.001 max:0.5;
	parameter "grass_flammability" category: "Grass" var: grass_flammability min:0.0;
	parameter "wildfire_rate" category: "Fire" var: wildfire_rate min:0.0;
	
	// Define attributes, actions, a init section and behaviors if necessary
	
	
	
	output {
		display "Number of trees" {
			chart "Number of adult trees" type: series size: {1,0.5} position: {0, 0} {
        		data "Number of araucaria trees" value: nb_araucaria color: #darkgreen ;
        		data "Number of broadleaf trees" value: nb_broadleaf color: #red ;
        	}
        }
        
		display "Radius adults a" {
			chart "Distance from center of adult a trees" type: series size: {1,0.5} position: {0, 0} {
        		data "araucaria trees - 05" value: rad_A_adults[0] color: #darkgreen ;
        		data "araucaria trees - 50" value: rad_A_adults[1] color: #darkgreen ;
        		data "araucaria trees - 95" value: rad_A_adults[2] color: #darkgreen ;
        	}
        } 
        display "Radius adults b" {
			chart "Distance from center of adult b trees" type: series size: {1,0.5} position: {0, 0} {
        		
        		data "broadleaf trees - 00" value: rad_B_adults[0] color: #purple ;
        		data "broadleaf trees - 50" value: rad_B_adults[1] color: #purple ;
        		data "broadleaf trees - 95" value: rad_B_adults[2] color: #purple ;
        	}
        } 
//        display "Radius all a" {
//			chart "Distance from center of all a trees" type: series size: {1,0.5} position: {0, 0} {
//        		data "araucaria trees - 0" value: rad_A_all[0] color: #darkgreen ;
//        		data "araucaria trees - 5" value: rad_A_all[1] color: #darkgreen ;
//        		data "araucaria trees - 50" value: rad_A_all[2] color: #darkgreen ;
//        		data "araucaria trees - 95" value: rad_A_all[3] color: #darkgreen ;
//        		data "araucaria trees - 100" value: rad_A_all[4] color: #darkgreen ;
//        	}
//        } 
//        display "Radius all b" {
//			chart "Distance from center of all b trees" type: series size: {1,0.5} position: {0, 0} {
//        		
//        		
//        		data "broadleaf trees - 0" value: rad_B_all[0] color: #purple ;
//        		data "broadleaf trees - 5" value: rad_B_all[1] color: #purple ;
//        		data "broadleaf trees - 50" value: rad_B_all[2] color: #purple ;
//        		data "broadleaf trees - 95" value: rad_B_all[3] color: #purple ;
//        		data "broadleaf trees - 100" value: rad_B_all[4] color: #purple ;
//        	}
//        } 
        
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