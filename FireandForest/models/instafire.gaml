/***
* Name: araucariaandfire
* Author: oz
* Description: 
* Tags: Tag1, Tag2, TagN
***/

model instafire

global {
	float landscape_size <- 300#m;
	float tile_size <- 10#m;
	geometry shape <- square(landscape_size);
	
	float initial_forest_size <- 100#m;
	
	// toggle phenomena
	bool shade_kills_grass <- true;
	bool wildfires <- true;
	string topography <- "plain";
	
	// general parameters
	float biomass_loss_burning <- 0.8;
	float minimum_biomass <- 0.1;
	// float carrying_capacity <- 1.0; // normalized
	
	int size <- round(landscape_size/tile_size);
	int initial_tree_pop <- 100;
	
	// Grass parameters
	float grass_growthrate <- 0.1;
	float grass_chance_to_start_fire <- 0.0001;
	float grass_flamability_ratio <- 1.0;
	
	// Tree parameters
	float shade_threshold <- 1.0;
	float shade_effect <- 0.0;
	float tree_dispersal <- 30#m parameter: true;
	
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
		/*
		create umbroph number: initial_tree_pop{
			stage <- 3;
			location <- any_location_in(c);
		}
		* */
	}

}


grid grass height:size width:size neighbors: 4 {
	
	float biomass <- 1.0; // this value is normalized to be in range (0,1]
	float carrying_capacity <- 1.0; //dependant on shade
	float flamability <- grass_flamability_ratio*biomass update: grass_flamability_ratio*biomass;
	float growthrate <- grass_growthrate;
	int last_burned <- 20 update: last_burned +1;
	
	float altitude;
	
	init {
		switch topography {
			match "plain"  {altitude <-0.0; }
			match "valley" {altitude <- ((location.x - 150)/10)^2 + ((location.y - 150)/20)^2;}
			match "mount"  {altitude <- 300-(((location.x - 150)/10)^2 + ((location.y - 150)/20)^2);}
		}
	}
	
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
		if(shade_kills_grass){
			int n_shade <- (here sum_of(each.stage));
			carrying_capacity <- 1.0/(0.2*n_shade+1);
		}
		else {
			carrying_capacity <- 1.0;
		}
	}
		
	reflex plant_growth{
		if(biomass <= minimum_biomass) {biomass<-minimum_biomass;}
		biomass <- biomass * (1+growthrate * (1 - biomass/carrying_capacity));
	}
	
	reflex start_fire when: wildfires and flip(grass_chance_to_start_fire) /*and flip(flamability)*/ {
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
	}
	
	// this will be used by default "grid" display
	rgb color<- rgb(255*(biomass),255*(biomass),0) 
		update: last_burned <1 ? #red : rgb(255*(biomass),255*(biomass),0,0.5);
		
	aspect trid {
		draw shape color: color depth: altitude;
	}
}


species tree {
	grass place;
	float height <- 0.0;
	list<agent> neighbors;
	float shade_ratio <- 1.0;
	int stage <- 0;
	
	list<float> reproduction_rate <- [0,0,0,0,0.688+0.071];
	list<float> death_rate <- [0.1,0.01,0.01,0.01,0.001];
	list<float> growth_rate <- [0.01,0.01,0.01,0.01,0];
	list<float> flamability <- [1.0,1.0,0.7,0.3,0.1];
	list<float> canopy_size <- [0.1,0.5,1,2,5];
	
	
	reflex place_me when: place = nil{
		place <- first (grass overlapping self);
		if(place=nil) {do die;}
		place.here <- place.here + self;
	}
	
	reflex shade {
		int n_shade <- (place.here count(each.stage > self.stage));
		if(n_shade) > shade_threshold {
			shade_ratio <- shade_effect;
		}
	
	}
	
	reflex grow when: flip(growth_rate[stage]*shade_ratio) and stage <4{
		stage <- stage + 1;
	}
	
	action burn {
		if(flip(flamability[stage])){
			place.here <- place.here - self;
			do die;
		}
	}
	
	reflex natural_death when: flip(death_rate[stage]){
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

species umbroph parent:tree {
    list<float> reproduction_rate <- [0,0,0,0,0.688+0.071];
	list<float> death_rate <- [1.0,0.8,0.1,0.1,0.02];
	list<float> growth_rate <- [0.1,0.1,0.1,0.1,0];
	list<float> flamability <- [1.0,1.0,1.0,1.0,1.0];
	
	reflex natural_death when: flip(death_rate[stage]*shade_ratio){
		place.here <- place.here - self;
		do die;
	}
	
	aspect default {
		draw circle(0.1+stage/2) color: rgb(245,255,0,0.5) border:#black;
	}
	
}

experiment instafire type: gui {

	
	// Parameters
	parameter "Landscape size" category: "Init" var: initial_forest_size min:0.0;
	parameter "Tile size" category: "Init" var: tile_size min:1#m;
	parameter "Initial tree pop" category: "Init" var: initial_tree_pop min:0;
	parameter "Initial forest size" category: "Init" var: initial_forest_size min:0.0;
	parameter "Topography" category:"Init" var: topography <- "???" among: ["plain","valley","mount"];
	
	parameter "Wildfires" category: "Toggle phenomena" var:wildfires;
	parameter "Shade kills grass" category: "Toggle phenomena" var:shade_kills_grass;
	
	parameter "Grass growth rate" category: "Grass" var: grass_growthrate min:0.001 max:0.5;
	parameter "Chance of fire" category: "Grass" var: grass_chance_to_start_fire min:0.0;
	parameter "Grass flamability" category: "Grass" var: grass_flamability_ratio min:0.0;
	
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
			species umbroph;
			//event mouse_up action: click;
		}
		
		display "model 3D" camera_interaction:false camera_pos:{world.shape.width/2,world.shape.height*2,world.shape.width*2} 
		camera_look_pos:{world.shape.width/2,world.shape.height/2,0} 
		camera_up_vector:{0.0,-1.0,0.0}type:opengl{
			species grass aspect: trid;
		}
	}
}