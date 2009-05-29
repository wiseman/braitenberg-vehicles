package com.ibm.jikes.skij;
import java.util.Hashtable;

/* This file is part of Skij.
 * Author: Michael Travers (mt@watson.ibm.com)
 * 
 * Licensed Materials - See the file license.txt.
 * (c) Copyright IBM Corp. 1997, 1998. All rights reserved.
 */

public abstract class Environment {

  public static Environment top;

  public Environment parent;

  // was static initializer
  static void initialize() {
    top = new TopEnvironment();
  }
  
  public abstract void addBinding(Symbol name, Object value) ;

  public Object getBinding(Symbol name) throws SchemeException {
    return getBinding(name, GET_REGULAR);
  }

  public Object getDynamicBinding(Symbol name) throws SchemeException {
    return getBinding(name, GET_DYNAMIC);
  }

  public Object getLocalBinding(Symbol name) throws SchemeException {
    return getBinding(name, GET_LOCAL);
  }
  
  static final int GET_REGULAR = 1;
  static final int GET_DYNAMIC = 2;
  static final int GET_LOCAL = 3;

  public abstract Object getBinding(Symbol name, int lookupType) throws SchemeException ;
  
  // return null instead of error if unbound (note: value may also be null)
  public Object getBindingSafe(Symbol name) {
    Object val;
    try { val = getBinding(name); }
    catch (SchemeException e) {
      return null;
    }
    return val;
  }

  public abstract void setBinding(Symbol name, Object newValue) throws SchemeException ;
  
  public abstract void forBindings(Procedure proc) throws SchemeException ;

  // it is really pretty hopeless trying to use functional style in Java. This
  // value should be a local, but inner classes can't access locals of enclosing methods.
  // So, instead use this global scratch variable and synchronize the method.
  static Cons bindings;
  public synchronized Cons getLocalBindings() {
    bindings = Nil.nil;
    // +++ this can be static now too
    Procedure proc = new PrimProcedure() {
      public Object apply(Environment env, Cons args) throws SchemeException {
	args.cdr = new Cons(getLocalBinding((Symbol)args.car)); // this modifes arglist! 
	bindings = new Cons(args, bindings);
	return null;
      }
    };
    try {
      forBindings(proc);
    }
    catch (SchemeException e) {} // ought not happen
    return bindings;
  }

  // +++ unused?
  //  public static final Symbol dynamicEnvironment = Symbol.intern("dynamic-environment");

  // go up the dynamic chain until we hit an Evaluator.
  abstract public TopEnvironment getTopEnvironment() ;  

}

