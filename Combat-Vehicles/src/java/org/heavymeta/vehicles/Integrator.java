package org.heavymeta.vehicles;

import java.util.*;
import java.lang.*;
import java.lang.Math.*;


/**
  Integrator

  A node that has digital inputs and an analog output, and whose
  output is a running average of the input.
  */

public class Integrator extends NodeWithInputs implements java.io.Serializable {
  public double decay_factor = 0.0;
  
  public double compute_output () {
		double input_sum = sum_inputs();
    double norm_decay_factor = Math.pow(decay_factor, (5.0 / world.ticks_per_second));

    return  ((Math.min(1.0, input_sum) * (1.0 - norm_decay_factor)) +
             (cached_output * norm_decay_factor));
  }
}


