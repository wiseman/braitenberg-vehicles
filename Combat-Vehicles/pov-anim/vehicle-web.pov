#include "colors.inc"
#include "shapes.inc"
#include "textures.inc"

#declare rear_hub_radius         = 0.06
#declare rear_inner_wheel_radius = .525
#declare rear_tire_radius        = .75
#declare rear_hub_extend         = 0.05
#declare rear_wheel_width        = .45

#declare rear_wheel_radius       = rear_tire_radius
#declare rear_hub_width 	 = rear_wheel_width + rear_hub_extend



#declare rear_wheel_cutout =
cylinder {
	<0, 0, 0>
	<rear_wheel_width, 0, 0>
	rear_tire_radius
	translate <-rear_wheel_width/2, 0, 0>
}

#declare rear_wheel =
merge {
	difference {
		/* tire */
		cylinder {
			<0, 0, 0>
			<rear_wheel_width, 0, 0>
			rear_tire_radius
		
			texture {
				pigment { color rgb <0.01, 0.01, 0.01> }
				finish { diffuse 0.3 }
				finish { phong 0.8 phong_size 3 }
					
			}
		}
		cylinder {
		 	<0, 0, 0>
			<rear_wheel_width * 2, 0, 0>
			rear_inner_wheel_radius
		}
	}

	difference {
		/* inner wheel */
		cylinder {
			<0, 0, 0>
			<rear_wheel_width, 0, 0>
			rear_inner_wheel_radius

			texture {
				Chrome_Texture
				pigment { color rgb <0.2, 0.2, 0.2> }
				/* normal { dents 0.1 } */
			}
		}
		cylinder {
			<0, 0, 0>
			<rear_wheel_width * 2, 0, 0>
			rear_hub_radius
		}

	}
	/* hub */
	cylinder {
		<0, 0, 0>
		<rear_hub_width, 0, 0>
		rear_hub_radius
		texture { Bronze_Metal }
	}
	translate <-rear_wheel_width/2, 0, 0>
}



#declare bcr = 0.2
#declare ba = <-1+bcr, 0+bcr, -2+bcr>
#declare bb = <1-bcr, 0+bcr, -2+bcr>
#declare bc = <-1+bcr, .75-bcr, -2+bcr>
#declare bd = <1-bcr, .75-bcr, -2+bcr>
#declare be = <-1+bcr, 0+bcr, 2-bcr>
#declare bf = <1-bcr, 0+bcr, 2-bcr>
#declare bg = <-1+bcr, .75-bcr, 2-bcr>
#declare bh = <1-bcr, .75-bcr, 2-bcr>


#declare body = 
difference {
	union {
		box {
			<-1, 0+bcr, -2+bcr>
			<1, .75-bcr, 2-bcr>
		}
		box {
			<-1+bcr, 0, -2+bcr>
			<1-bcr, .75, 2-bcr>
		}
		box {
			<-1+bcr, 0+bcr, -2>
			<1-bcr, .75-bcr, 2>
		}
		sphere { ba bcr }
		sphere { bb bcr }
		sphere { bc bcr }
		sphere { bd bcr }
		sphere { be bcr }
		sphere { bf bcr }
		sphere { bg bcr }
		sphere { bh bcr }

		cylinder { ba bb bcr }
		cylinder { ba bc bcr }
		cylinder { ba be bcr }
		cylinder { bb bd bcr }
		cylinder { bb bf bcr }
		cylinder { bc bd bcr }
		cylinder { bc bg bcr }
		cylinder { bd bh bcr }
		cylinder { be bf bcr }
		cylinder { be bg bcr }
		cylinder { bf bh bcr }
		cylinder { bg bh bcr }

		texture {
			Polished_Chrome
			pigment {
				color rgb <0.6 0.6 0.6>
			}
		}
	}

	object {
		rear_wheel_cutout
		translate <-1.05, 0.375, -1.05>
	}
	object {
		rear_wheel_cutout
		translate <1.05, 0.375, -1.05>
	}
	object {
		rear_wheel_cutout
		translate <-1.05, 0.375, -.85>
	}

	object {
		rear_wheel_cutout
		translate <1.05, 0.375, -.85> 
	}
}


#declare caster_wheel =
cylinder {
	<-.05, 0, 0>
	<.05,  0, 0>
	.15
	texture {
		Phong_Dull
		pigment {
			color rgb <0.3, 0.3, 0.3>
		}
	}
}

#declare caster_support =
object {
	triangle {
		<0, .375, .05>,
		<0, .375, -.05>,
		<0, .1, 0>
	}
	texture {
		Polished_Chrome
		pigment { color rgb <0.4 0.3 0.3> }
	}
}


#declare caster =
union {
	object {
		caster_wheel
		translate <0, 0.15, 0>
	}
	object {
		caster_support
		translate <-.0501, 0, 0>
	}
	object {
		caster_support
		translate <.0501, 0, 0>
	}
}


#declare sensor = 
intersection {
	object {
		Paraboloid_Y
		scale <1, 7, 1>
	}
	object {
		plane { <0, 1, 0> 0.33
			texture {
				finish { reflection 0.7 }
				pigment { color rgb <0.8 0.4 0.4> }
			}
		}
	}
	rotate <90, 0, 0>
}


#declare car = 
union {
	object {
		body
		translate <0, 0.375, 0>
	}

	object {
		sensor
		rotate <0, -20, 0>
		translate <-.7, 1.1, 1.8>
	}

	object {
		sensor
		rotate <0, 20, 0>
		translate <.7, 1.1, 1.8>
	}

	object {
		caster
		translate <.5, 0, 1.5>
	}

	object {
		caster
		translate <-.5, 0, 1.5>
	}

	object {
		rear_wheel
		rotate <0, 180, 0>
		translate <-1.05, 0.75, -.95>
	}

	object {
		rear_wheel
		translate <1.05, 0.75, -.95>
	}
}



plane {
	<0, 1, 0> 0
	texture {
		Glossy
		pigment {
			color rgb <0.1 0.1 0.3>
		}
	}
}

object {
	car
	rotate <0, -130, 0>
	translate <0, 0, 2>
}

/*
object {
	car
	rotate <0, 145, 0>
	translate <-3, 0, 7>
}

object {
	car
	rotate <0, -110, 0>
	translate <3, 0, 8.5>
}
*/

light_source { <0, 4, 2> color White}
light_source { <-3, 4, 7> color White}
light_source { <3, 4, 8.5> color White}
light_source { <5, 4, 2> color White}
light_source { <2, 2, 8> color White}
light_source { <0, .2, -4> color White}

light_source { <5, 2, -4> color White}

/*
light_source { <0, 4, -3> color White}
light_source { <0, 4, 0> color White}
light_source { <-2, 4, -3> color White}
light_source { <2, 4, 3> color White}
light_source { <0, 1, -2> color White }
*/

camera {
	location  <4, 3.5, -3>
        look_at   <0, 1,  2>
}
