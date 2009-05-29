package org.heavymeta.vehicles;

import java.lang.*;
import java.util.*;


public class Gun extends Neurode implements java.io.Serializable {
	public double range = 3.0;
	public boolean fired_flag = false;
	public Orientation relative_orientation = new Orientation();
	public Location relative_location = new Location();
	public Platform platform;
	
    public Location location() {
      return (platform.location()).add(relative_location);
	}
  
    public Orientation orientation() {
      return (platform.orientation()).add(relative_orientation);
    }
	
	public boolean firing() {
	  return this.output() > 0.0;
	}
	
	public void apply_to_vehicle(TwoWheeledVehicle v) {
		damage_node(v.random_node(), v);
	}
	
	public void damage_node(Node n, TwoWheeledVehicle v) {
	}
	
	public TwoWheeledVehicle vehicle_hit_by(World w) {
	Enumeration vs;
	TwoWheeledVehicle v;
	
	vs = w.vehicles.elements();
	while (vs.hasMoreElements()) {
	  v = (TwoWheeledVehicle) vs.nextElement();
	  if (gun_hit_vehicle_p(v)) {
	  	return v;
	  }
	 }
	 return null;
	}
	
	
	public boolean gun_hit_vehicle_p(TwoWheeledVehicle target) {
		Location gun_loc = this.location();
		Location target_loc = target.location();
		double theta = this.orientation().theta;
		double x0, y0, xr, yr, xc, yc, a, b, c;
		
		x0 = gun_loc.x;
		y0 = gun_loc.y;
		xr = Math.cos(theta);
		yr = Math.sin(theta);
		xc = target_loc.x;
		yc = target_loc.y;
		
		a = (xr * xr) + (yr * yr);
		b = (2 * (x0 - xc) * xr) + (2 * (y0 - yc) * yr);
		c = (square(x0 - xc) + square(y0 - yc)) -
		    square(target.length / 2.0);
		return positive_root_p(a, b, c, this.range);
	}
	
	public boolean positive_root_p(double a, double b, double c, double range) {
	  if (c == 0.0) {
	  	return (range > (-c / b)) && ((-c / b) > 0.0);
	  } else {
	  	double disc = (b * b) - (4 * a * c);
	  	if (disc < 0.0) {
	  	  return false;
	  	} else {
	  	  double discrt = Math.sqrt(disc);
	  	  double d1, d2;
	  	  d1 = (discrt - b) / (a * 2.0);
	  	  d2 = (discrt + b) / (a * 2.0);
	  	  return ((range > d1 && d1 > 0) || (range > d2 && d2 > 0));
	  	}
	  }
	}
	
	private static double square(double x) {
	  return x * x;
	}
}
