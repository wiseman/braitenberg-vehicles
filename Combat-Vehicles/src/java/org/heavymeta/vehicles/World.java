package org.heavymeta.vehicles;

import java.util.*;
import java.lang.*;
import java.awt.*;
import java.io.*;


public class World implements Serializable {
    public long time = 0;
    public int ticks_per_second = 100;
    public Vector vehicles = new Vector();
    public Vector lamps = new Vector();
    public Vector radiators = new Vector();
    public Vector sensors = new Vector();
    public IsAHierarchy radiation_hierarchy = new IsAHierarchy();
    
    public Enumeration radiators_of_type(String type) {
	return new EnumerateRadiators(this, radiators, type);
    }		
    
    
    public static World load(File f)
	throws com.ibm.jikes.skij.SchemeException, FileNotFoundException
    {
	return (new Parser()).parseWorldFile(f);
    }
    
    private void step_internal(long n) {
	long i;
	Enumeration vs;
	
	for (i = 0; i < n; i++) {
	    vs = vehicles.elements();
	    while (vs.hasMoreElements()) {
		((TwoWheeledVehicle) vs.nextElement()).move();
		time +=1;
	    }
	}
    }
    
    public synchronized void step() {
	step_internal(1);
    }
    
    public synchronized void step(long n) {
	step_internal(n);
    }
    
}



 
