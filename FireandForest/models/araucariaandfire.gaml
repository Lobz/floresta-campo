/***
* Name: araucariaandfire
* Author: oz
* Description: 
* Tags: Tag1, Tag2, TagN
***/

model araucariaandfire

global {
	geometry shape <- square(300#m);
	float initial_forest_size <- 100#m;
	float neighborhood <- 10#m;
	
	// general parameters
	float biomass_loss_burning <- 0.8;
	float minimum_biomass <- 0.0001;
	// float carrying_capacity <- 1.0; // normalized
	
	// Araucaria tree parameters
	float tree_adult_height <- 20#m; // minimum height to be an adult
	float tree_max_height <- 40#m;
	float tree_deathrate <- 0.0 parameter: true;
	float tree_reproductionrate <- 0.3 parameter: true;
	float tree_growthrate <- 0.01;
	
	// Shade-tolerant forest parameters
	
	
	// Grass parameters
	float grass_growthrate <- 0.02;
	float grass_chance_to_start_fire <- 0.0015;
	float grass_flamability_ratio <- 0.7;
	
	init {
		geometry c <- circle(100);
		create tree number:1 {
			location <- any_location_in(c);
		}
	}
}

species plant schedules: shuffle(plant){
	float biomass <- 1.0; // this value is normalized to be in range (0,1]
	float height <- 0.0;
	float flamability; // different for each type of plant
	float chance_to_start_fire; // different for each type of plant
	float growthrate; // different for each type of plant
	list<plant> neighbors; // different for each type of plant
	float carrying_capacity <- 1.0; //dependant on shade
	list<tree> my_trees;
	float shade;
	
	bool burning <- false;
	
	
		
	reflex plant_growth when: not(burning) {
		if(biomass <= 0.0) {biomass<-minimum_biomass;}
		biomass <- biomass * (1+growthrate * (1 - biomass/carrying_capacity));
	}
	
	reflex start_fire when: not(burning) and flip(chance_to_start_fire) /*and flip(flamability)*/ {
		burning <- true;
	}
	
	reflex catch_fire when: not(burning) {
		loop n over: neighbors {
			if (n.burning and flip(flamability)){
				burning <- true;
			}
		}
	}
	
	reflex burn when:burning {
		biomass <- biomass*biomass_loss_burning;
	}
	
	reflex stop_burning when:burning {
		if(flip(1-flamability)){
			burning <- false;
		}
	}
	
}


grid grass height:30 width:30 neighbors: 8 parent:plant {
	float flamability <- grass_flamability_ratio*biomass update: grass_flamability_ratio*biomass;
	float chance_to_start_fire <- grass_chance_to_start_fire; 
	float growthrate <- grass_growthrate;
	
	reflex set_shade {
		my_trees <- (tree inside(self)) where (each.height > 1.0);
		shade <- length(my_trees)/1.0;
		carrying_capacity <- shade > 1 - minimum_biomass ? minimum_biomass : 1-shade;
	}
	
	// this will be used by default "grid" display
	rgb color<- rgb(255*(biomass),255*(biomass),0) 
		update: burning ? #red : rgb(255*(biomass),255*(biomass),0);
}

species tree parent: plant {
	float flamability <- 1-biomass update: 1-biomass; 
	float chance_to_start_fire <- 0.0; 
	float growthrate <- tree_growthrate; 
	
	float height <- biomass*tree_max_height update: max(height,biomass*tree_max_height);
	float deathrate <- tree_deathrate*(1.0-0.99*biomass) update: tree_deathrate*(1.0 - 0.99*biomass);
	grass my_plot <- first( grass overlapping self);
	
	reflex find_neighbors {
		neighbors <- plant inside(my_plot) + my_plot + my_plot.neighbors;
	}
	
	reflex set_shade {
		my_trees <- (tree inside my_plot) where (each.height > height);
		shade <- length(my_trees)/8.0;
		carrying_capacity <- max(minimum_biomass,1-shade);
	}
	
	reflex die {
		if (flip(deathrate)){
			do die;
		}
	}
	
	reflex reproduce when: height > tree_adult_height{
		if( flip(tree_reproductionrate)){
			create tree {
				geometry circ <- circle(1) translated_to myself.location;
				write circ.location;
				write circ;
				location <- myself.location + {rnd(-30.0,30.0),rnd(-30.0,30.0)};//any_location_in(circ);
			}
		}
	}
	
	aspect default {
		draw circle(2.0) color: burning? #orange : rgb(0,255*biomass,0) border:#black;
	}
}

experiment araucaria type: gui {

	
	// Define parameters here if necessary
	parameter "Grass growth rate" category: "My parameters" var: grass_growthrate min:0.001 max:0.5;
	parameter "Tree growth rate" category: "My parameters" var: tree_growthrate min:0.001 max:0.5;
	parameter "Chance of fire" category: "My parameters" var: grass_chance_to_start_fire min:0.0;
	parameter "Grass flamability" category: "My parameters" var: grass_flamability_ratio min:0.0;
	
	// Define attributes, actions, a init section and behaviors if necessary
	
	
	
	output {
	// Define inspectors, browsers and displays here
	
	// inspect one_or_several_agents;
	//
		display "model" {
			grid grass;
			species tree;
		}
	}
}