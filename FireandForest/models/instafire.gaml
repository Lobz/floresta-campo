/***
* Name: araucariaandfire
* Author: oz
* Description: 
* Tags: Tag1, Tag2, TagN
***/

model instafire

global {
	
	geometry shape <- square(300#m);
	
	float initial_forest_size <- 100#m parameter: true;
	
	// general parameters
	float biomass_loss_burning <- 0.8;
	float minimum_biomass <- 0.1;
	// float carrying_capacity <- 1.0; // normalized
	
	int size <- 60;
	int initial_tree_pop <- 100;
	
	// Grass parameters
	float grass_growthrate <- 0.1;
	float grass_chance_to_start_fire <- 0.0001;
	float grass_flamability_ratio <- 0.8;
	
	// Tree parameters
	float tree_adult_height <- 0.5; // minimum height to be an adult
	float tree_max_height <- 1;
	float tree_deathrate <- 0.01 parameter: true;
	float tree_growthrate <- 0.01;
	float shade_threshold <- 1.0 parameter: true;
	float shade_effect <- 0.0 parameter: true;
	float tree_dispersal <- 10#m;
	
	// Monitoring
	int nb_trees -> {length(tree)};

	action click {
		grass selected <- first(grass overlapping #user_location);
		ask selected {do catch_fire;}
	}
	
	init {		
		geometry c <- circle(initial_forest_size);
		create tree number: initial_tree_pop{
			stage <- 3;
			location <- any_location_in(c);
		}
	}

}


grid grass height:size width:size neighbors: 4 {
	
	float biomass <- 1.0; // this value is normalized to be in range (0,1]
	float carrying_capacity <- 1.0; //dependant on shade
	float flamability <- grass_flamability_ratio*biomass update: grass_flamability_ratio*biomass;
	float growthrate <- grass_growthrate;
	int last_burned <- 20 update: last_burned +1;
	
	float altitude <- ((location.x - 150)/10)^2;
	
	float spread_chance(grass a,grass b) {
		float distab <- abs(a.location.x-b.location.x) + abs(a.location.y - b.location.y);
		float slope <- (b.altitude - a.altitude)/(distab);
		
		// I want a function that is equal do 1 when slope is 0.4 (aprox 20 degrees)
		if(slope >= 0.4) {
			return 1.0;
		}
		if(slope <= -0.4) {
			return 0.2;
		}
		
		return 1.0*(slope+0.6);
	}
	
	list<tree> here;
	
	reflex shade {
		int n_shade <- (here sum_of(each.stage));
		carrying_capacity <- 1.0/(0.2*n_shade+1);
	}
		
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
		loop t over: here {ask t {do burn;}}
	}
	
	action spread_fire {
		list<grass> spread <- neighbors;
		list<float> spr_ch <- spread accumulate spread_chance(self, each);
		list<grass> done <- [self];
	
		int i <- 0;
		grass n;
		loop while: i < length(spread) {
			n <- spread[i];
			float chance <- spr_ch[i];
			if(not(done contains n)){
				if(flip(n.flamability * chance)){
					ask n {do burn;}
					list<grass> nxt <- (n.neighbors - done);
					spr_ch <- spr_ch + (nxt accumulate spread_chance(n,each));
					spread <- spread + nxt;
					done <- done + n;
				}
			}
			i<-i+1;
		}
		write(spr_ch);
	}
	
	// this will be used by default "grid" display
	rgb color<- rgb(255*(biomass),255*(biomass),0) 
		update: last_burned <1 ? #red : rgb(255*(biomass),255*(biomass),0,0.5);
}


species tree {
	grass place;
	float height <- 0.0;
	float flamability <- 1.0;
	float death_by_fire <- 1.0;
	list<tree> neighbors;
	float shade_ratio <- 1.0;
	int stage <- 0;
	
	list<float> reproduction_rate <- [0,0,0,0,0.688+0.071];
	
	reflex place_me when: place = nil{
		place <- first (grass overlapping self);
		if(place=nil) {do die;}
		place.here <- place.here + self;
	}
	
	reflex shade {
		int n_shade <- (place.here count(each.stage >= self.stage));
		if(n_shade) > shade_threshold {
			shade_ratio <- shade_effect;
		}
	
	}
	
	reflex grow when: flip(tree_growthrate*shade_ratio) and stage <4{
		stage <- stage + 1;
		death_by_fire <- 1.0-0.2*stage;
	}
	
	action burn {
		if(flip(flamability)){
			if(flip(death_by_fire)){
				write(stage);
				place.here <- place.here - self;
				do die;
			}
		}
		else {write("oops");}
	}
	
	reflex natural_death when: flip(tree_deathrate){
		place.here <- place.here - self;
		do die;
	}
	
	reflex reproduce when: flip(reproduction_rate[stage]){
		create tree {
			location <- myself.location + {rnd(-tree_dispersal,tree_dispersal),rnd(-tree_dispersal,tree_dispersal)};
			
		}
	}
	
	aspect default {
		draw circle(0.1+stage/2) color: rgb(0,255,0,0.5) border:#black;
	}
}

experiment instafire type: gui {

	
	// Define parameters here if necessary
	parameter "Grass growth rate" category: "My parameters" var: grass_growthrate min:0.001 max:0.5;
	parameter "Chance of fire" category: "My parameters" var: grass_chance_to_start_fire min:0.0;
	parameter "Grass flamability" category: "My parameters" var: grass_flamability_ratio min:0.0;
	parameter "Size" category: "Init" var: size min:3;
	parameter "Initial tree pop" category: "Init" var: initial_tree_pop min:0;
	
	// Define attributes, actions, a init section and behaviors if necessary
	
	
	
	output {
		
		
		monitor "Number of trees" value:nb_trees;
		display "charts" {
			chart "Species evolution" type: series size: {1,0.5} position: {0, 0} {
        		data "Number of trees" value: nb_trees color: #blue ;
        	}
        }
	// Define inspectors, browsers and displays here
	
	// inspect one_or_several_agents;
	//
		display "model" {
			grid grass;
			species tree;
			//event mouse_up action: click;
		}
	}
}