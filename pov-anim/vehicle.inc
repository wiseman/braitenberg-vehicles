/* Input variables

      car_color        -  A color
      car_orientation  -  A vector
      car_position     -  A vector

*/
      
/*
#include "colors.inc"
#include "shapes.inc"
#include "textures.inc"
#include "consts.inc"
*/

#include "metals.inc"

#declare rear_hub_radius         = 0.06
#declare rear_inner_wheel_radius = .525
#declare rear_tire_radius        = .75
#declare rear_hub_extend         = 0.05
#declare rear_wheel_width        = .45

#declare rear_wheel_radius       = rear_tire_radius
#declare rear_hub_width 	 = rear_wheel_width + rear_hub_extend

#declare sensor_x_offset         = .7
#declare sensor_z_offset         = 1.8

#declare front_wire_angle        = 27
#declare rear_wire_angle         = 150

#declare wire_texture            = texture { T_Chrome_5E }


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
/*				finish { diffuse 0.3 } */
  		      normal { dents 0.3 scale 0.00001 }
				finish { reflection 0 phong 0.1 phong_size 3 }
					
			}
		}
		cylinder {
		 	<-rear_wheel_width, 0, 0>
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
				finish { phong 0.8 phong_size 15 }
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
			pigment { color rgb <0.6 0.6 0.6> }
			finish { diffuse 0.4 }
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


/*
#declare caster_wheel =
cylinder {
	<-.05, 0, 0>
	<.05,  0, 0>
	.15
	texture {
		finish { Phong_Dull }
		pigment { color rgb <0.3, 0.3, 0.3> }
	}
}
*/

#declare caster_wheel =
merge {
	difference {
		/* tire */
		cylinder {
			<-.05, 0, 0>
			<.05, 0, 0>
			.15
		
			texture {
				pigment { color rgb <0.01, 0.01, 0.01> }
/*				finish { diffuse 0.3 } */
  		      normal { dents 0.4 scale 0.001 }
				finish { reflection 0 phong 0.3 phong_size 30 }
					
			}
		}
		cylinder {
		 	<-.1, 0, 0>
			<.1, 0, 0>
			.10
		}
	}

	/* inner wheel */
	cylinder {
		<-.05, 0, 0>
		<.05, 0, 0>
		.10
		texture {
			Chrome_Texture
			pigment { color rgb <0.2, 0.2, 0.2> }
			finish { phong 0.8 phong_size 3 }
			/* normal { dents 0.1 } */
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
		texture {
			Polished_Chrome
			pigment { color rgb <0.4 0.4 0.4> }
		}
	}
	object {
	sphere { <0, -.2, 0> .53 

/*		plane { <0, 1, 0> 0.33 */
			texture {
				finish { ambient 0.3 reflection 0.7 }
				pigment { color rgb <0.388 0.2 0.1> }
			}
		}
	}
	rotate <90, 0, 0>
}


#declare flag =
object {
    cylinder {
        <0, 1.125, 0>
        <0, 1.2, 0>
        .6
        texture { pigment { car_color } }
        finish { ambient 0.9 }
    }
}



#declare front_wire_length1 = sensor_x_offset/sin(radians(front_wire_angle))
#declare front_wire_z = front_wire_length1 * cos(radians(front_wire_angle))
#declare front_wire_length2 = sensor_z_offset - front_wire_z - bcr

#declare rear_wire_length1 = -.95 / cos(radians(rear_wire_angle))
#declare rear_wire_x = rear_wire_length1 * sin(radians(rear_wire_angle))
#declare rear_wire_length2 = (1 - .02) - rear_wire_x - bcr


#declare solder_connector =
object {
    sphere {
        <0, 0, 0>
        .04
	texture { wire_texture }
	finish { ambient 0.9 }
    }
}

#declare wire_joint = 
object {
    sphere {
        <0, 0, 0>
        .02
        texture { wire_texture }
        finish { ambient 0.9 }
    }
}

#declare left_front_wire =
union {
    /* Front left, from flag. */
    cylinder {
        <0, 0, 0>
        <0, front_wire_length1, 0>
        .02
        texture { wire_texture }
        finish { ambient 0.9 }
        rotate <90, -front_wire_angle, 0>
        translate <0, 1.120, 0>
    }

    /* Front left wire joint */
    object {
		wire_joint
        translate <-.7, 1.120, front_wire_z>
    }

    /* Front left sensor connector */    
	object {
		solder_connector
		translate <-sensor_x_offset, 1.120, sensor_z_offset - bcr>
    }

    /* Front left, from sensor. */
    cylinder {
        <0, 0, 0>
        <0, front_wire_length2, 0>
        .02
        texture { wire_texture }
        finish { ambient 0.9 }
        rotate <90, 0, 0>
        translate <-.7, 1.120, front_wire_z>
    }
}

#declare right_front_wire =
object {
	left_front_wire
	scale <-1, 1, 1>
}


#declare left_rear_wire =
union {
    /* Rear left, from flag. */
    cylinder {
        <0, 0, 0>
        <0, rear_wire_length1, 0>
        .02
        texture { wire_texture }
        finish { ambient 0.9 }
        rotate <90, -rear_wire_angle, 0>
        translate <0, 1.120, 0>
    }
    
    /* Rear left wire joint */
    object {
		wire_joint
        translate <-rear_wire_x, 1.120, -.95>
    }

    /* Rear left, to motor. */
    cylinder {
        <0, 0, 0>
        <0, rear_wire_length2, 0>
        .02
        texture { wire_texture }
        finish { ambient 0.9 }
        rotate <90, -90, 0>
        translate <-rear_wire_x , 1.120, -.95>
    }

	object {
		solder_connector
		translate <-rear_wire_x - rear_wire_length2, 1.12, -.95>
	}
}

#declare right_rear_wire =
object {
	left_rear_wire
	scale <-1, 1, 1>
}

	
#declare wires =
union {
	object { left_front_wire }
	object { right_front_wire }
	object { left_rear_wire }
	object { right_rear_wire }
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
		translate <-sensor_x_offset, 1.1, sensor_z_offset>
	}

	object {
		sensor
		rotate <0, 20, 0>
		translate <sensor_x_offset, 1.1, sensor_z_offset>
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
	
	object {
	        flag
	}

	object {
	        wires
	}
}


object {
	car
	texture { pigment { car_color } }
	scale <.15, .15, .15>
	rotate car_orientation + <0, 90, 0>
	translate car_position*2
}



