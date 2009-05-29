package org.heavymeta.vehicles;

import java.io.*;


public class Location implements Serializable {
	public double x = 0.0;
	public double y = 0.0;

	public Location() {}
	
  public Location(double x, double y) {
    this.x = x;
    this.y = y;
  }
	
	public double square_distance(Location l) {
		double dx = x - l.x;
		double dy = y - l.y;
		return (dx * dx) + (dy * dy);
	}
	
	public Location add(Location l) {
		Location new_l = new Location();
		new_l.x = x + l.x;
		new_l.y = y + l.y;
		return new_l;
	}
	
	public Orientation orientation_between(Location l) {
		Orientation o = new Orientation();
		o.theta = Math.atan2(l.y - y, l.x - x);
		return o;
	}
	
	public String toString() {
		return new String("[Location " + x + " " + y + "]");
	}
}

