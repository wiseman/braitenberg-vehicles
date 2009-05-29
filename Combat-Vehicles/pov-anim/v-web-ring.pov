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
// light_source { <0, .2, -4> color White}
// light_source { <0, 3, 3> color White}
// light_source { <5, 2, -4> color White}

#declare car_position = <1, 0, 0>
#declare car_orientation = <0, 180, 0>
#declare car_color = color rgb <0, 1, 0>
#include "vehicle.inc"


#declare Seed1 = seed(0)
#declare Radius=1
#declare Count = 1
#while (Count < 24)
	#declare Angle = Count * 15
	#declare RAngle = Angle + ((rand(Seed1) - 0.5) * 2)
	#declare RRadius = Radius + ((rand(Seed1) - 0.5) / 5) * Radius
	#declare car_position = <cos(radians(RAngle))*RRadius, 0, sin(radians(RAngle))*RRadius>
	#declare car_orientation = <0, 180-(RAngle + (rand(Seed1) - 0.5) * 10), 0>
	#declare car_color = color rgb <1, 0, 0>
	#include "vehicle.inc"
	#declare Count = Count + 1
#end

light_source { <.4, 4, -3> color White}

/*
camera {
	direction <0, 0, FoV_90>
	location  <0, 5, 0>
        look_at   <0, 0, 0>
}
*/



camera {
	direction <0, 0, FoV_90>
	location  <1.6, .25, -0.19>
        look_at   <1.7, 0, 0>
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


#declare lamp_position = <0, 0, 0>
#declare lamp_color = color rgb <1, 1, 0.2>
#include "lamp.inc"

