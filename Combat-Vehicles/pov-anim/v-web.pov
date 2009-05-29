#include "colors.inc"
#include "shapes.inc"
#include "textures.inc"
#include "consts.inc"



/*
object {
	car
	rotate <0, 180, 0>
	translate <0, 0, 2>
}
*/

light_source { <0, 4, 2> color White}
light_source { <-3, 4, 7> color White}
light_source { <2, 2, 8> color White}
light_source { <0, .2, -4> color White}
light_source { <0, 3, 3> color White}
light_source { <5, 2, -4> color White}


#declare car_position = <0, 0, .02>
#declare car_orientation = <0, 40, 0>
#declare car_color = color rgb <.32, .33, .15>
#include "vehicle.inc"

#declare car_position = <.5, 0, -.25>
#declare car_orientation = <0, 250, 0>
#declare car_color = color rgb <1, 0, 0>
#include "vehicle.inc"

/*
object {
	car
	rotate <0, 45, 0>
	translate <1, 0, -5>
}
*/

light_source { <.4, 4, -3> color White}

camera {
	direction <0, 0, FoV_60>
	location  <1.2, .37, -.1>
        look_at   <0, 0, 0>
}


plane {
	<0, 1, 0> 0
	texture {
		finish { Glossy }
		pigment {
			color rgb <0.1 0.1 0.3>
		}
	}
}


