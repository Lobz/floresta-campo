/***
* Name: araucariaandfire
* Author: oz
* Description: 
* Tags: Tag1, Tag2, TagN
***/

model instafire

global {
	// general parameters
	float biomass_loss_burning <- 0.8;
	float minimum_biomass <- 0.1;
	// float carrying_capacity <- 1.0; // normalized
	
	// Grass parameters
	float grass_growthrate <- 0.02;
	float grass_chance_to_start_fire <- 0.0;
	float grass_flamability_ratio <- 0.7;

	action click {
		grass selected <- first(grass overlapping #user_location);
		ask selected {do burn;}
	}

}


grid grass height:10 width:10 neighbors: 4 {
	
	float biomass <- 1.0; // this value is normalized to be in range (0,1]
	float carrying_capacity <- 1.0; //dependant on shade
	float flamability <- grass_flamability_ratio*biomass update: grass_flamability_ratio*biomass;
	float chance_to_start_fire <- grass_chance_to_start_fire; 
	float growthrate <- grass_growthrate;
	bool burning <- false;
	int last_burned <- 20 update: last_burned +1;
	
		
	reflex plant_growth{
		if(biomass <= minimum_biomass) {biomass<-minimum_biomass;}
		biomass <- biomass * (1+growthrate * (1 - biomass/carrying_capacity));
	}
	reflex start_fire when: flip(chance_to_start_fire) /*and flip(flamability)*/ {
		do burn;
	}
	
	action burn {
		burning <- true;
		biomass <- biomass*biomass_loss_burning;
	
		loop n over: neighbors {
			if(not(n.burning) /*and flip(n.flamability)*/){
				ask n {do burn;}
			}
		}
		
		burning <- false;
		last_burned <- 0;
	}
	
	// this will be used by default "grid" display
	rgb color<- rgb(255*(biomass),255*(biomass),0) 
		update: last_burned <2 ? #red : rgb(255*(biomass),255*(biomass),0);
}

experiment araucaria type: gui {

	
	// Define parameters here if necessary
	parameter "Grass growth rate" category: "My parameters" var: grass_growthrate min:0.001 max:0.5;
	parameter "Chance of fire" category: "My parameters" var: grass_chance_to_start_fire min:0.0;
	parameter "Grass flamability" category: "My parameters" var: grass_flamability_ratio min:0.0;
	
	// Define attributes, actions, a init section and behaviors if necessary
	
	
	
	output {
	// Define inspectors, browsers and displays here
	
	// inspect one_or_several_agents;
	//
		display "model" {
			grid grass;
		}
	}
}