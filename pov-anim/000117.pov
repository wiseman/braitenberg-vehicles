// #<POV-RENDERER #x1B0F36E> Time: 3.92  Frame: 0
#include "colors.inc"
#include "shapes.inc"
#include "textures.inc"
#include "consts.inc"
global_settings {
        ambient_light color rgb <0.50000, 0.50000, 0.50000>
         assumed_gamma 1.0
}

plane {
	<0, 1, 0> 0
	texture {
		finish { Glossy }
		pigment { color rgb <0.1 0.1 0.3> }
	}
}


/* possible axes */


camera {
	direction <0.00000, 0.00000, FoV_60>
	location  <3.50000, 2.15000, -3.50000>
	look_at   <0.00000, 1.00000, 2.00000>
}


light_source { <0, 10, 0> color rgb <.5, .5, .5> }
light_source { <0, 4, 2> color White}
light_source { <-3, 4, 7> color White}
light_source { <2, 2, 8> color White}
light_source { <0, .2, -4> color White}
light_source { <0, 3, 3> color White}
light_source { <5, 2, -4> color White}



#declare car_position = <0, 0, 2>
#declare car_orientation = <0, 180, 0>
#declare car_color = color<.65, .77, .29>
#include "vehicle.inc"

union {
	sphere { <-1000, -1000, -1000> 1 }
	sphere { <-1000, -1000, -1000> 1 }
	sphere { <-1000, -1000, -1000> 1 }
}


// #<LAMP [(-1.50, 1.50) Brightness: 0.60] #x1B0E62E>

#declare lamp_position = <-1.50000, 0.00000, 1.50000>
#declare lamp_color = color rgb <0.55826, 0.62028, 0.55826>
#include "lamp.inc"

// #<LAMP [(1.50, 1.50) Brightness: 0.01] #x1B0E3E6>

#declare lamp_position = <1.50000, 0.00000, 1.50000>
#declare lamp_color = color rgb <0.00574, 0.00574, 0.00637>
#include "lamp.inc"

// #<LAMP [(0.00, -1.50) Brightness: 0.45] #x1B0E19E>

#declare lamp_position = <0.00000, 0.00000, -1.50000>
#declare lamp_color = color rgb <0.45315, 0.40783, 0.40783>
#include "lamp.inc"

