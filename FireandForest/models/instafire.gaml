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
	
	int size <- 30;
	
	// Grass parameters
	float grass_growthrate <- 0.1;
	float grass_chance_to_start_fire <- 0.0001;
	float grass_flamability_ratio <- 0.7;

	action click {
		grass selected <- first(grass overlapping #user_location);
		ask selected {do catch_fire;}
	}

}


grid grass height:size width:size neighbors: 4 {
	
	float biomass <- 1.0; // this value is normalized to be in range (0,1]
	float carrying_capacity <- 1.0; //dependant on shade
	float flamability <- grass_flamability_ratio*biomass update: grass_flamability_ratio*biomass;
	float growthrate <- grass_growthrate;
	int last_burned <- 20 update: last_burned +1;
	
		
	reflex plant_growth{
		if(biomass <= minimum_biomass) {biomass<-minimum_biomass;}
		biomass <- biomass * (1+growthrate * (1 - biomass/carrying_capacity));
	}
	reflex start_fire when: flip(grass_chance_to_start_fire) /*and flip(flamability)*/ {
		do catch_fire;
	}
	
	action catch_fire {	
		do burn;
		do spread_fire;
	}
	
	action burn {
		biomass <- biomass*biomass_loss_burning;
		last_burned <- 0;
	}
	
	action spread_fire {
		list<grass> spread <- neighbors;
		list<grass> done <- self;
	
		int i <- 0;
		grass n;
		loop while: i < length(spread) {
			n <- spread[i];
			if(not(done contains n)){
				if(flip(n.flamability)){
					ask n {do burn;}
					spread <- spread + (n.neighbors - done);
					done <- done + n;
					}
			}
			i<-i+1;
		}
	}
	
	// this will be used by default "grid" display
	rgb color<- rgb(255*(biomass),255*(biomass),0) 
		update: last_burned <1 ? #red : rgb(255*(biomass),255*(biomass),0);
}

experiment instafire type: gui {

	
	// Define parameters here if necessary
	parameter "Grass growth rate" category: "My parameters" var: grass_growthrate min:0.001 max:0.5;
	parameter "Chance of fire" category: "My parameters" var: grass_chance_to_start_fire min:0.0;
	parameter "Grass flamability" category: "My parameters" var: grass_flamability_ratio min:0.0;
	parameter "Size" category: "My parameters" var: size min:3;
	
	// Define attributes, actions, a init section and behaviors if necessary
	
	
	
	output {
	// Define inspectors, browsers and displays here
	
	// inspect one_or_several_agents;
	//
		display "model" {
			grid grass;
			event mouse_up action: click;
		}
	}
}