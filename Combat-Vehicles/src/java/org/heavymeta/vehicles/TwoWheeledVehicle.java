package org.heavymeta.vehicles;

import java.lang.*;
import java.util.*;
import java.io.*;


public class TwoWheeledVehicle extends Platform implements Serializable {
	public double max_speed = 10.0;
	public double wheel_radius = 0.05;
	public double wheel_base = 0.3;
	public double length = 0.6;
	public Motor right_motor;
	public Motor left_motor;
	public Vector guns = new Vector();
	public boolean fired_flag = false;
	public Double last_move_time;
	public double left_wheel_rotation_angle = 0.0;
	public double right_wheel_rotation_angle = 0.0;

	public String toString() {
		return new String("[Vehicle " + location() + orientation() + "]");
	}
	
	public TwoWheeledVehicle() {
		color = new java.awt.Color(255, 0, 0);
	}
	
	public void move() {
		if (last_move_time == null || last_move_time.doubleValue() < world.time) {
			last_move_time = new Double(world.time);
			double max_dist, dist_right, dist_left, theta, x, y, dist_diff;
			double delta_theta, turn_radius, new_theta, new_x, new_y;
			Location location;
			Orientation orientation;

			orientation = orientation();
			location = location();
			
			max_dist = max_speed / world.ticks_per_second;
			dist_right = right_motor.output() * max_dist;
			dist_left = left_motor.output() * max_dist;
			theta = orientation.theta;
			x = location.x;
			y = location.y;
			old_location.x = x;
			old_location.y = y;
			dist_diff = dist_right - dist_left;
			
//			System.out.println("dist_diff: " + dist_diff);
			
			delta_theta = dist_diff / wheel_base;
			if (Math.abs(dist_diff) < .0001) {
				turn_radius = 0.0;
			} else {
				turn_radius = (dist_right / delta_theta) - (wheel_base / 2);
			}

//			System.out.println("turn_radius: " + turn_radius);

			new_theta = theta + delta_theta;
			if (turn_radius == 0.0) {

//				System.out.println("turn_radius == 0");
				
				new_x = x + Math.cos(theta) * dist_left;
				new_y = y + Math.sin(theta) * dist_left;
			} else {

//				System.out.println("new_theta= " + new_theta + " theta= " + theta);

				new_x = x + ((Math.sin(new_theta) - Math.sin(theta)) * turn_radius);
				new_y = y - ((Math.cos(new_theta) - Math.cos(theta)) * turn_radius);
			}
			orientation.theta = new_theta;
			location.x = new_x;
			location.y = new_y;
			
			maybe_fire_guns();
		}
	}
	
	public Node random_node() {
		return null;
	}
	
	public void maybe_fire_guns() {
		Enumeration gs;
		Gun gun;
		TwoWheeledVehicle v;
		
		gs = this.guns.elements();
		while (gs.hasMoreElements()) {
			gun = (Gun) gs.nextElement();
			if (gun.firing()) {
				gun.fired_flag = true;
				v = gun.vehicle_hit_by(world);
				if (!(v == null)) {
					gun.apply_to_vehicle(v);
				}
			}
		}
	}
}

