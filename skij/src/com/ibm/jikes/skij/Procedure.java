package com.ibm.jikes.skij; 

/* This file is part of Skij.
 * Author: Michael Travers (mt@watson.ibm.com)
 * 
 * Licensed Materials - See the file license.txt.
 * (c) Copyright IBM Corp. 1997, 1998. All rights reserved.
 */

/** 
 * Abstract class for procedures.
 */
public abstract class Procedure {

  /**
   * The name of this procedure (may be null).
   */
  public Symbol name;

  abstract String shortClassName();

  /**
   * Apply the procedure to the arguments within the given environment.
   */
  public abstract Object apply(Environment within, Cons args) throws SchemeException;

  /**
   * Apply the procedure to the arguments within the global environment.
   */
  public Object apply(Cons args) throws SchemeException {
    return apply(Environment.top, args);
  }

  /**
   * A convenience method: apply the procedure to <code>args</code> in the top-level
   * environment, catching exceptions.
   */
  public Object safeApply(Cons args) {
    try {
      return apply(Environment.top, args);
    }
    catch (SchemeException e) {
      System.out.println("error within " + this + ".safeApply: " + e.toString());      
      return null;
    }
  }

  public String toString() {
    if (name == null)
      return "[" + shortClassName() + " <anon> @" + Integer.toHexString(hashCode()) + "]";
    else
      if (this == Environment.top.getBindingSafe(name)) {
	return "[" + shortClassName() + " " + name + "]"; } // top-level procs don't get hash id
      else
	return "[" + shortClassName() + " " + name + " @" + Integer.toHexString(hashCode()) + "]";
  }

  /**
   * Register the procedure within environment <code>env</code>.
   */
  public void define(Environment env) {
    env.addBinding(name, this);
  }

}

  
