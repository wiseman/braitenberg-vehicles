#declare real_lamp_color = lamp_color*3 + <.1, .1, .1>

#declare lamp = sphere {
	lamp_position + <0, .1125, 0>
	.165
	texture {
		pigment {color real_lamp_color}
		finish {ambient 0.8 diffuse 0.6}
	}
}

light_source { lamp_position color real_lamp_color looks_like { lamp } }


