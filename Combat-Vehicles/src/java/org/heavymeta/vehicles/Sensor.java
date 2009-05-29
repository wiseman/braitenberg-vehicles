package org.heavymeta.vehicles;

import java.util.*;
import java.lang.*;
import java.lang.Math.*;
import java.io.*;


/**
  Sensor
  
  Sensors on platforms.  Sensors take an analog input corresponding
  to the experienced intensity of the radiation to which they are
  sensitive.  They have a digital output whose rate is proportional
  to the radiation intensity.
  */

public class Sensor extends Node implements Serializable {
  public String radiation_type;
  public double sensitivity = 1.0;
  public boolean directional_p = true;
  public double field_of_view = 0.0;
  public Orientation relative_orientation = new Orientation();
  public Location relative_location = new Location();
  public Platform platform;
  public double accumulator = 0.0;
  
	public String toString() {
		return new String("[Sensor " + radiation_type + "]");
	}

  public double compute_output () {
    double rate, accum, pulse;
    
    rate = sensitivity * sum_inputs();
    accum = Math.min(1.0, accumulator + rate);
    if (accum >= 1.0) {
    	accum -= 1.0;
    	pulse = 1.0;
    } else {
    	pulse = 0.0;
    }
    accumulator = accum;
    return pulse;
  }

  /** Returns the sum of the received signal intensities.
   */
  public double sum_inputs() {
		double sum = 0.0;

//		System.out.println("Sensor: sum_inputs");
		
		Enumeration rads = world.radiators_of_type(radiation_type);
		while (rads.hasMoreElements()) {
			sum += signal_strength((Radiator) rads.nextElement());
		}
		return sum;
	}
  
  /** Calculates the radiation received by a sensor from a radiator.
   */
  public double signal_strength (Radiator radiator) {
//		System.out.println("Sensor: signal_strength");
		// Assume sensors are shielded from radiators on the same platform.
    if (radiator.platform == platform) {
//			System.out.println("same platform");
      return 0.0;
    } else {
			Location lp, sp;
			double sd, i;
			
			lp = radiator.location();
			sp = this.location();
			sd = sp.square_distance(lp);
			i = Math.cos(this.orientation().angle(sp.orientation_between(lp)));
			if (directional_p) {
				return ((i + 1.0) * radiator.output()) / (sd * 2.0);
			} else {
        return radiator.output() / (sd * 2.0);
      }
    }
  }
  
  public Location location() {
    return (platform.location()).add(relative_location);
  }
  
  public Orientation orientation() {
    return (platform.orientation()).add(relative_orientation);
  }
}


