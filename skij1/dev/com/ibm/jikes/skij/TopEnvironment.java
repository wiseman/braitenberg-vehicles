package com.ibm.jikes.skij;
import java.util.*;

/* This file is part of Skij.
 * Author: Michael Travers (mt@watson.ibm.com)
 * 
 * Licensed Materials - See the file license.txt.
 * (c) Copyright IBM Corp. 1997, 1998. All rights reserved.
 */

/**
 * TopEnvironments represent the global binding environment. While there is
 * only one set of top-level bindings, there can be many TopEnvironment objects.
 * TopEnvironments also keep track of current input and output ports.
 */
public class TopEnvironment extends Environment {

  public InputPort in;
  public OutputPort out;

  static final int initialSize = 450;
  // all TopEnvironments share the same bindings!
  static Object[] bindings = new Object[initialSize];
  static int topIndex = 0;

  static Hashtable autoloadHT = new Hashtable();

  /** 
   * Create a TopEnvironment with ports based on a SchemeListener.
   */
  TopEnvironment(SchemeListener l) {
    in = new InputPort(l.in);
    out = new OutputPort(l.out);
  }

  /** 
   * Create a TopEnvironment with ports for the System (Java) console.
   */
  TopEnvironment() {
    in = new InputPort(System.in);
    out = new OutputPort(System.out);
  }


  public void addBinding(Symbol name, Object value) {
    addTopBinding(name, value);
  }

  static void addTopBinding(Symbol name, Object value) {
    int index = name.index;
    if (index == -1) {
      index = topIndex++;
      name.index = index;
      if (index == bindings.length) {
	Object[] newBindings = new Object[2 * index];
	System.arraycopy(bindings, 0, newBindings, 0, index);
	bindings = newBindings;
      }
    }
    bindings[index] = value; 
  }

  public void setBinding(Symbol name, Object value) throws SchemeException {
    getBinding(name);		// for null check
    addBinding(name, value);
  }

  // lookup type is ignored here
  public Object getBinding(Symbol name, int lookupType) throws SchemeException {
    try {
      return bindings[name.index];
    }
    catch (ArrayIndexOutOfBoundsException e) {
      String autoFile = (String)autoloadHT.get(name);
      if (autoFile == null)
	throw new SchemeException(name + " is unbound");
      else {
	try {
	  Scheme.loadResource("lib/" + autoFile + ".scm");
	  return bindings[name.index];
	}
	catch (Throwable ee) {
	  throw new SchemeException("autoload failed for " + name + ": " + ee);
	}
      }
    }
  }


  // we have no direct access to the list of toplevel bindings, so iterate over all 
  // symbols and pick the ones with toplevel definitions.
  public void forBindings(Procedure proc) throws SchemeException {
    Enumeration e = Symbol.namespace.elements();
    for (; e.hasMoreElements() ; ) {
      Symbol s = (Symbol)e.nextElement();
      if (s.index >= 0 ||
	  autoloadHT.get(s) != null)
	proc.apply(Environment.top, Cons.list(s));
    }
  }

  public TopEnvironment getTopEnvironment() {
    return this;
  }

  /**
   * Set up the autoload table. Called by init.scm
   */
  public static void initAutoload(Cons specs) {
    for (Cons rest = specs; rest != Nil.nil; rest = (Cons)rest.cdr) {
      Cons entry = (Cons)rest.car;
      setAutoLoad((Symbol)entry.car, (String)entry.cadr());
    }
  }
    
  /**
   * Set up a single autoload function. 
   */
  public static void setAutoLoad(Symbol name, String file) {
    autoloadHT.put(name, file);
  }

  /**
   * used by where-is procedure
   */
  public static String getAutoLoad(Symbol name) {
    return (String)autoloadHT.get(name);
  }

}
