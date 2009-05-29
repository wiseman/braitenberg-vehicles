package com.ibm.jikes.skij;
import java.util.Hashtable;

/* This file is part of Skij.
 * Author: Michael Travers (mt@watson.ibm.com)
 * 
 * Licensed Materials - See the file license.txt.
 * (c) Copyright IBM Corp. 1997, 1998. All rights reserved.
 */

/**
 * The abstract class representing Scheme environments. 
 */
public abstract class Environment {

  /**
   * The global environment.
   */
  public static Environment top;

  /**
   * This environment's parent (null in the case of a TopEnvironment).
   */
  public Environment parent;

  // was static initializer
  static void initialize() {
    top = new TopEnvironment();
    PrimProcedure.boxedBoolean(true);	// force this to be loaded, defining prims
  }
  
  /**
   * Add a binding.
   */
  public abstract void addBinding(Symbol name, Object value) ;

  /**
   * Return the value of a binding.
   */
  public Object getBinding(Symbol name) throws SchemeException {
    return getBinding(name, GET_REGULAR);
  }

  /**
   * Return the value of a binding using dynamic lookup (that is, chaining
   * through the environments of callers rather than through those of definers).
   */
  public Object getDynamicBinding(Symbol name) throws SchemeException {
    return getBinding(name, GET_DYNAMIC);
  }

  /**
   * Get a local binding (that is, only look in this environment, not in parents).
   */
  public Object getLocalBinding(Symbol name) throws SchemeException {
    return getBinding(name, GET_LOCAL);
  }
  
  static final int GET_REGULAR = 1;
  static final int GET_DYNAMIC = 2;
  static final int GET_LOCAL = 3;

  abstract Object getBinding(Symbol name, int lookupType) throws SchemeException ;
  
  /**
   * Get a binding, returning null if unbound. (note: value may also be null)
   */
  public Object getBindingSafe(Symbol name) {
    Object val;
    try { val = getBinding(name); }
    catch (SchemeException e) {
      return null;
    }
    return val;
  }

  /**
   * Change the value of an existing binding.
   */
  public abstract void setBinding(Symbol name, Object newValue) throws SchemeException ;
  
  /** 
   * Map a procedure over all bindings in this environment. The procedure is
   * called with the name of the binding (a symbol).
   */
  public abstract void forBindings(Procedure proc) throws SchemeException ;

  // it is really pretty hopeless trying to use functional style in Java. This
  // value should be a local, but inner classes can't access locals of enclosing methods.
  // So, instead use this global scratch variable and synchronize the method.
  static Cons bindings;

  synchronized Cons getLocalBindings() {
    bindings = Nil.nil;
    // +++ this can be static now too
    Procedure proc = new PrimProcedure() {
      public Object apply(Environment env, Cons args) throws SchemeException {
	bindings = new Cons(Cons.list(args.car, getLocalBinding((Symbol)args.car)), bindings);
	return null;
      }
    };
    try {
      forBindings(proc);
    }
    catch (SchemeException e) {} // ought not happen
    return bindings;
  }

  /**
   * Return the TopEnvironment for this environment.
   */
  abstract public TopEnvironment getTopEnvironment() ;  

}

