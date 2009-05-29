package org.heavymeta.vehicles;

import java.lang.*;
import java.util.*;
import java.io.*;

/** A non-mobile platform.
 */

public class Lamp extends Platform implements Serializable {

	public Lamp() {
		color = new java.awt.Color(255, 255, 0);
	}

	public double brightness() {
		double sum = 0.0;
		Radiator r;
		
  	Enumeration rads = world.radiators.elements();
  	while (rads.hasMoreElements()) {
			r = (Radiator) rads.nextElement();
			if (!(r.platform == this)) {
	  		sum += r.output();
	  	}
  	}
  	return sum;
  }
}
		
	
				
