package org.heavymeta.vehicles;

import java.util.*;
import java.lang.*;
import java.lang.Math.*;


/**
  Node with inputs
  
  Any node that accepts inputs.
  */

public abstract class NodeWithInputs extends Node {
	public Vector current_inputs = new Vector();
  public Vector original_inputs = new Vector();

	public Vector inputs() {
		return current_inputs;
	}
	
  public double sum_inputs() {
		return sum_node_values(current_inputs);
  }

  
  static double sum_node_values(Vector nodes) {
		double sum = 0.0;
  	Enumeration ns = nodes.elements();
  	while (ns.hasMoreElements()) {
  		sum += ((Node) ns.nextElement()).output();
  	}
  	return sum;
  }
}



	
