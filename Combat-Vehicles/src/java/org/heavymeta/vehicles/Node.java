package org.heavymeta.vehicles;

import java.util.*;
import java.lang.*;
import java.lang.Math.*;


/**
  Nodes
  
  Nodes in a nervous system.  Correctly handle asynchronous updating.
  */

public abstract class Node extends NamedObject {
  public double cached_output = 0.0;
  public long cache_timestamp = -1;
  public Vector probes = new Vector();
  
  /**
    Returns the node's output, only recomputing if necessary otherwise using a
    cached value.  A node's output value only needs to be computed once per tick
    of the simulator clock.
    */
  public double output() {
    double cached_value = cached_output;
    long world_time = world.time;

//		System.out.println("Node.output: " + this.toString() +
//									     "(" + world_time + ")" + 
//									     "(" + cache_timestamp + ")");
    /* Only recompute what the output should be if we haven't
       computed and cached it already */
    if (cache_timestamp != world_time) {
//			System.out.println("  Calling compute_output()");
      cache_timestamp = world_time;
      cached_output = compute_output();
    }
    return cached_value;
  }
  
  /** 
    Forces a node's output to be recomputed, and returns the result. Child
    classes should specialize this method.
    */
  public abstract double compute_output ();
  
}



