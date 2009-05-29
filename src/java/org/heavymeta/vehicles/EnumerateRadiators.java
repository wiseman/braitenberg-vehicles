package org.heavymeta.vehicles;

import java.util.*;
import java.lang.*;
import java.io.*;


public class EnumerateRadiators implements Enumeration, Serializable {
	public String type;
	public Vector radiators;
	public World world;
	public int numRads;
	public int pos;
	
	public EnumerateRadiators(World world, Vector radiators, String type) {
		this.world = world;
		this.type = type;
		this.radiators = radiators;
		numRads = radiators.size();
		pos = 0;
	}
	
	public boolean hasMoreElements() {
		while ((pos < numRads) && !(world.radiation_hierarchy.isA(((Radiator) radiators.elementAt(pos)).radiation_type, type)))
			pos++;
		return (pos < numRads);
	}
	
	public Object nextElement() {
		if (hasMoreElements()) {
			pos++;
			return radiators.elementAt(pos-1);
		} else {
			return null;
		}
	}
}

