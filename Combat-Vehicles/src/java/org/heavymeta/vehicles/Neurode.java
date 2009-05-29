package org.heavymeta.vehicles;

import java.util.*;
import java.lang.*;
import java.lang.Math.*;


/**
  Neurode
  
  Nodes that act as computational elements.
  */

public class Neurode extends NodeWithInputs implements java.io.Serializable {
  public double threshold = 0.0;
  public Vector inhibitors = new Vector();
  
  public double sum_inhibitors() {
  	return sum_node_values(inhibitors);
  }

  public double compute_output() {
		double input_sum = sum_inputs();
		double inhib_sum = sum_inhibitors();
	
		if ((inhib_sum <= 0.0) && (input_sum >= threshold)) {
			return 1.0;
		} else {
			return 0.0;
		}
  }
}


