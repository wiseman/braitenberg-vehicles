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


#declare car_position = <8, 0, -5>
#declare car_orientation = <0, 250, 0>
#declare car_color = color rgb <.5, 0, 0>
#include "vehicle.inc"

#declare car_position = <-13, 0, 19>
#declare car_orientation = <0, 95, 0>
#declare car_color = color rgb <0, .5, 0>
#include "vehicle.inc"

#declare car_position = <4, 0, 4>
#declare car_orientation = <0, 175, 0>
#declare car_color = color rgb <0, 0, .5>
#include "vehicle.inc"


/*
light_source { <0, 4, 2> color White}
light_source { <-3, 4, 7> color White}
light_source { <2, 2, 8> color White}
light_source { <0, .2, -4> color White}
light_source { <0, 3, 3> color White}
light_source { <5, 2, -4> color White}
*/


#declare lamp_position = <2, 0, -3.7>
#declare lamp_color = color rgb <1.0, 1.0, 0.0>
#include "lamp.inc"

#declare lamp_position = <-4, 0, 9>
#declare lamp_color = color rgb <0.0, 0.0, 1.0>
#include "lamp.inc"


/*
light_source { <0, 1000, 0> color rgb <.5, .5, .5>}
light_source { <100, 10, 0> color rgb <.5, .5, .5>}
light_source { <18.5, 6.15, -12> color rgb <.5, .5, .5>}
*/


object {
	box {
		<.1, .1, .1>
		<100, .2, .2>
	}
	pigment { color rgb <1, 0, 0> }
}

object {
	box {
		<.1, .1, .1>
		<.2, 100, .2>
	}
	pigment { color rgb <0, 1, 0> }
}

object {
	box {
		<.1, .1, .1>
		<.2, .2, 100>
	}
	pigment { color rgb <0, 0, 1> }
}


camera {
	direction <0, 0, FoV_60>
	location  <18.5, 6.15, -12>
	look_at   <0, 1,  2>
}