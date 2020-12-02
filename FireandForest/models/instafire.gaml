/***
* Name: araucariaandfire
* Author: oz
* Description: 
* Tags: Tag1, Tag2, TagN
***/

model instafire

global {
	// spatial scale vars
	float landscape_size <- 600#m;
	float tile_size <- 10#m;
	
	// derived spatial scale vars
	geometry shape <- square(landscape_size);
	int size <- round(landscape_size/tile_size);
	float tile_area <- tile_size^2;
	
	// toggle phenomena
	bool wildfires <- true;
	string topography <- "plain";
	
	// general parameters
	float chance_to_start_fire <- 0.1;
	
	// Initial forest parameters
	int initial_pop_araucaria <- 100;
	int initial_pop_umbroph <- 100;
	float initial_forest_size <- 50#m;
	int initial_tree_stage <- 4;
	
	// Grass parameters
	float grass_growthrate <- 0.5;
	float grass_flamability <- 0.9;
	float biomass_loss_burning <- 0.8;
	float minimum_biomass <- 0.1;
	// float carrying_capacity <- 1.0; // normalized
	
	// Tree parameters
	float shade_threshold_araucaria <- 1.0;
	float shade_threshold_umbroph <- 3.0;
	float shade_effect_base <- 0.0;
	float shade_effect_umbroph <- 0.0;
	float tree_dispersal <- 10#m;
	float umb_dispersal <- 10#m;

	
	// Monitoring
	int nb_araucaria -> {length(araucaria where (each.stage > 3))};
	int nb_umbroph -> {length(umbroph where (each.stage > 3))};
	int firesize -> {first(wildfire).firesize};
	float forest_radius (list<tree> valid_trees) {
		int n95 <- round(length(valid_trees)*0.95);
		float radius <- 0.0;
		geometry forest <- circle(radius);
		
		loop while: length(valid_trees overlapping forest) < n95 {
			radius <- radius + 1.0;
			forest <- circle(radius);
		}
		return radius;
	}
	
	float rad_h -> forest_radius(araucaria where (each.stage > 3));
	float rad_u -> forest_radius(umbroph where (each.stage > 3));
	
	init {		
		geometry c <- circle(initial_forest_size);
		create umbroph number: initial_pop_umbroph{
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

species scheduler schedules: wildfire + shuffle(grass) + shuffle(umbroph + araucaria); // explicit scheduling in the world

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
	float shade_effect <- shade_effect_base;
	float n_shade <-0.0;
	float my_dispersal <- tree_dispersal;
	float my_shade_threshold <- shade_threshold_araucaria;
	rgb my_color <- rgb(0,255,0,0.5);
	
	list<float> reproduction_rate <- [0,0,0,0,0.688+0.071];
	list<float> death_rate <- [0.9,0.1,0.01,0.01,0.003];
	list<float> growth_rate <- [0.1,0.1,0.1,0.1,0];
	list<float> flamability <- [1.0,1.0,0.7,0.3,0.1];
	list<float> canopy_size <- [0.1,0.5,1,2,5];
	list<float> height <- [0.1,0.5,1.0,10,40];
	
	float my_reproduction_rate;
	float my_death_rate;
	float my_growth_rate;
	float my_flamability;
	float my_canopy_size;
	float my_height;
	float my_canopy_area;
	geometry my_canopy;
	geometry shape <- circle(0.5); // radius of firecatching?
	
	action update_traits {
		self.my_reproduction_rate <- self.reproduction_rate[self.stage];
		self.my_death_rate <- self.death_rate[self.stage];
		self.my_growth_rate <- self.growth_rate[self.stage] * self.shade_ratio;
		self.my_flamability <- self.flamability[self.stage];
		self.my_canopy_size <- self.canopy_size[self.stage];
		self.my_height <- self.height[self.stage];
		self.my_canopy_area <- 3.14*self.my_canopy_size^2;
		self.my_canopy <- circle(my_canopy_size);
		self.places <- grass overlapping my_canopy;
	}
	
	action real_init {
		place <- first (grass overlapping self);
		if(place=nil) {do die;}
		place.here <- place.here + self;
		
		places <- grass overlapping my_canopy;
		do get_shade();
	}
	
	action get_shade {
		if (place.shade_over_height(my_height) > my_shade_threshold) {
			self.shade_ratio <- shade_effect;
		}
		do update_traits();
	}
	
	reflex shade {
		do get_shade();
	}
	
	reflex grow when: flip(my_growth_rate){
		stage <- stage + 1;
		do update_traits();
	}
	
	action burn {
		if(flip(my_flamability)){
			place.here <- place.here - self;
			do die;
		}
	}
	
	reflex natural_death when: flip(my_death_rate){
		place.here <- place.here - self;
		do die;
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
	
	reflex reproduce when: flip(my_reproduction_rate){
		create tree {
			location <- myself.disperse();
			do real_init;
		}
	}	
	
		
	aspect default {
		draw my_canopy color: my_color border:#black;
	}
}

species araucaria parent:tree schedules: []{
	
	reflex reproduce when: flip(reproduction_rate[stage]){
		create araucaria {
			location <- myself.disperse();
			do real_init;
		}
	}	
}

species umbroph parent:tree schedules: []{
	list<float> flamability <- [1.0,1.0,1.0,1.0,1.0];
	float shade_effect <- shade_effect_umbroph;
	float my_shade_threshold <- shade_threshold_umbroph;
	float my_dispersal <- umb_dispersal;
	rgb my_color <- rgb(245,0,0,0.5);
	
	reflex reproduce when: flip(reproduction_rate[stage]){
		create umbroph {
			location <- myself.disperse();
			do real_init;
		}
	}

	
}


experiment instafire type: gui {

	
	// Parameters
	parameter "Landscape size" category: "Init" var: landscape_size min:0.0;
	parameter "Patch size" category: "Init" var: initial_forest_size min:0.0;
	parameter "Tile size" category: "Init" var: tile_size min:1#m;
	parameter "Initial light demanding pop" category: "Init" var: initial_pop_araucaria min:0;
	parameter "Initial shade tolerant pop" category: "Init" var: initial_pop_umbroph min:0;
	parameter "Initial forest size" category: "Init" var: initial_forest_size min:0.0;
	parameter "Average araucaria dispersal" category: "Init" var: tree_dispersal min:0.0;
	parameter "Average umbrophile dispersal" category: "Init" var: umb_dispersal min:0.0;
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
        		data "Number of umbroph trees" value: nb_umbroph color: #red ;
        	}
        }
        
		display "Radius of circle with 95% of adult trees" {
			chart "Radius of circle with 95% of adult trees" type: series size: {1,0.5} position: {0, 0} {
        		data "araucaria trees" value: rad_h color: #darkgreen ;
        		data "Umbroph trees" value: rad_u color: #red ;
        	}
        }
        
        display "Wildfires" {
			chart "Size of the last fire" type: series style: stack size: {1,0.5} position: {0, 0} y_range: {0,size^2} {
        		data "Number of terrain tiles" value:firesize color: #black ;
        	}
        }
	
		display "model" {
			grid grass;
			species umbroph;
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

experiment fireandforest type: headless {
	parameter "Landscape size" category: "Init" var: landscape_size min:0.0;
	parameter "Patch size" category: "Init" var: initial_forest_size min:0.0;
	parameter "Tile size" category: "Init" var: tile_size min:1#m;
	parameter "Initial Araucaria pop" category: "Init" var: initial_pop_araucaria min:0;
	parameter "Initial broadleaved pop" category: "Init" var: initial_pop_umbroph min:0;
	parameter "Initial forest size" category: "Init" var: initial_forest_size min:0.0;
	parameter "Average araucaria dispersal" category: "Init" var: tree_dispersal min:0.0;
	parameter "Average umbrophile dispersal" category: "Init" var: umb_dispersal min:0.0;
	parameter "Topography" category:"Init" var: topography among: ["plain","valley","ridge"];
	
	parameter "Wildfires" category: "Fire" var:wildfires;
	parameter "Chance of fire" category: "Fire" var: chance_to_start_fire min:0.0;
	
	output {

    	monitor "Number of araucaria trees" value: nb_araucaria;
    	monitor "Number of broadleaved trees" value: nb_umbroph;
		monitor "Circle size for araucaria trees" value: rad_h;
        monitor "Circle size for broadleaved trees" value: rad_u;
		monitor "Size of fire" value:firesize;
		
		display "display" {
			grid grass;
			species umbroph;
			species araucaria;
		}
	}
}