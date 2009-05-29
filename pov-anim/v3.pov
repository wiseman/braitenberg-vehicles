// #<WORLD time: 0, TPS: 100, 2 vehicles, 5 radiators #x31EF42E>

#include "colors.inc"
#include "shapes.inc"
#include "textures.inc"
#include "consts.inc"


global_settings {
	ambient_light color rgb <0.5, 0.5, 0.5>
	assumed_gamma 1.0
}


plane {
	<0, 1, 0> 0
	texture {
		finish { Glossy }
		pigment { color rgb <0.1 0.1 0.3> }
	}
}



object {
	box { <0, -.05, -.05> <100, .05, .05> }
	pigment { color rgb <1, 0, 0> }
}

/*
object {
	box { <0, 0, 0> <.1, 100, .1> }
	pigment { color rgb <0, 1, 0> }
}
*/

object {
	box { <-.05, -.05, 0> <.05, .05, 100> }
	pigment { color rgb <0, 0, 1> }
}

camera {
	direction <0, 0, FoV_30>
	location  <0, 2, 0>
	look_at   <0, 0, 0>
}

light_source { <3, 1.2, 3> color 1}



// #<TWO-WHEELED-VEHICLE [(2.00, 3.00) 3.14 0.00 units/s] #x31F014E>

#declare car_position = <2.0, 0.0, 3.0>
#declare car_orientation = <0.0, 180.0, 0.0>
#declare car_color = color rgb <1.0, 0, 0>
#include "vehicle.inc"





#declare lamp_position = <1, 0.0, 0>
#declare lamp_color = color rgb <1.0, .7, 1.0>
#include "lamp.inc"


#declare lamp_position = <2, 0.0, 0>
#declare lamp_color = color rgb <0.0, 0.0, 0.0>
#include "lamp.inc"

#declare lamp_position = <1.5, 0.0, 3>
#declare lamp_color = color rgb <.7, 1.0, .7>
#include "lamp.inc"


/*
#declare car_position = <1.5, 0.4, 2.1>
#declare car_orientation = <180.0, 0, 0.0>
#declare car_color = color rgb <0, 1.0, 0>
#include "vehicle.inc"
*/

#declare car_position = <0, 0, 0>
#declare car_orientation = <0.0, 0.0, 0.0>
#declare car_color = color rgb <1.0, 0, 0>
#include "vehicle.inc"

/*
#declare car_position = <3.0, 0.0, 1.0>
#declare car_orientation = <0.0, 90, 0.0>
#declare car_color = color rgb <0, 1.0, 0>
#include "vehicle.inc"
*/