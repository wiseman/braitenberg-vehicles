package org.heavymeta.vehicles;

import java.util.*;
import java.lang.*;
import java.lang.Math.*;
import java.io.*;

/**
  Radiator
  
  A source of radiation.
  */

public class Radiator extends Integrator implements Serializable {
	public String radiation_type;
	public double brightness = 1.0;
	public Platform platform;

	public Location location() {
		return platform.location();
	}
	
	public double compute_output() {
		
		if ((inputs().isEmpty()) && (original_inputs.isEmpty())) {
			// If we never had any inputs then output is fixed.
			return brightness;
		} else {
			// Call Integrator's method.
			return super.compute_output();
		}
	}
	
	public String toString() {
		return new String("[Radiator " + radiation_type + "]");
	}
}


