package org.heavymeta.vehicles;

import java.io.*;


public class Orientation implements Serializable {
	public double theta = 0.0;

  public Orientation() { }

  public Orientation(double theta) {
    this.theta = theta;
  }

	public Orientation add(Orientation o) {
		Orientation new_o = new Orientation();
		new_o.theta = theta + o.theta;
		return new_o;
	}
	
	public double angle (Orientation o) {
		return o.theta - theta;
	}		
	
	public String toString() {
		return new String("[Orientation " + (theta * (180.0 / 3.1416)) + "]");
	}
}

