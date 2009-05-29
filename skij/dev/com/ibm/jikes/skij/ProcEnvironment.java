package com.ibm.jikes.skij; 
import java.util.Hashtable;

/* This file is part of Skij.
 * Author: Michael Travers (mt@watson.ibm.com)
 * 
 * Licensed Materials - See the file license.txt.
 * (c) Copyright IBM Corp. 1997, 1998. All rights reserved.
 */

/** 
 * Environments for procedures (or any non-toplevel environment).
 */
public class ProcEnvironment extends Environment {

  Object names = Nil.nil; // these two probably should not be public once reflection is fixed
  Cons values = Nil.nil;
  public Environment dynamicParent = null;
  
  public ProcEnvironment() {
  }

  /** 
   * Create an environment with p as the parent.
   */
  public ProcEnvironment(Environment p) {
    parent = p;
  }

  ProcEnvironment(Environment p, Environment dp) {
    // here's a kludge to prevent frame build-up
    if (dp.parent == p && p instanceof ProcEnvironment)// is this a recursion?
      dp = ((ProcEnvironment)dp).dynamicParent;	// then don't leave calling frame in dynamic path
    parent = p; dynamicParent = dp;
  }

  // quicky!
  ProcEnvironment(Environment p, Object argnames, Cons argvals) {
    parent = p;
    names = argnames;
    values = argvals;
  }

  ProcEnvironment(Environment p, Environment dp, Object argnames, Cons argvals) {
    // here's a kludge to prevent frame build-up
    if (dp.parent == p && p instanceof ProcEnvironment)// is this a recursion?
      dp = ((ProcEnvironment)dp).dynamicParent;	// then don't leave calling frame in dynamic path
    parent = p;
    dynamicParent = dp;
    names = argnames;
    values = argvals;
  }

  public void addBinding(Symbol name, Object newValue) {
    try {
      setBinding(name, newValue, true);
    }
    catch (SchemeException e) {
      System.out.println("this can't happen"); // or so I hope
    }
  }

  public void setBinding(Symbol name, Object newValue) throws SchemeException {
    setBinding(name, newValue, false);
  }

  // does a define or set based on define argument
  public void setBinding(Symbol name, Object newValue, boolean define) throws SchemeException {
    Object restNames;
    Cons restValues;
    Cons localNil = Nil.nil;
    for (restNames = names, restValues = values;
	 restNames != localNil; 
	 restNames = ((Cons)restNames).cdr, restValues = (Cons)restValues.cdr)
      {
        if (restNames instanceof Symbol) {
	  if (restNames == name) {
	    replaceRestBinding(name, newValue);
	    return;
	  }
	  else
	    break;
	}
	else if (((Cons)restNames).car == name) {
	  ((Cons)restValues).car = newValue;
	  return;
	}
      }

    if (define) {
      names = new Cons(name, names);
      values = new Cons(newValue, values);      
    }
    else {
      if (parent != null)
	parent.setBinding(name, newValue);
      else
	throw new SchemeException("can't set unbound variable " + name);
    }
  }
    
  // remove the rest binding (which must be present). Assumes you are going to add a regular
  // binding for the removed symbol
  void replaceRestBinding(Symbol name, Object value) {
    if (names instanceof Symbol) 
      names = Nil.nil;
    else {
      Cons restNames;
      Cons restValues;
      for (restNames = (Cons)names, restValues = values;;
	   restNames = (Cons)restNames.cdr, restValues = (Cons)restValues.cdr)
	if (restNames.cdr instanceof Symbol) {
	  // don't do this, because the name structure is shared! leaving the extra name around should have no effect
	  // restNames.cdr = Nil.nil;
	  restValues.cdr = Nil.nil;
	  break;
	}
    }
    names = new Cons(name, names);
    values = new Cons(value, values);
  }
    

  public Object getBinding(Symbol name, int lookupType) throws SchemeException {
    Object restNames;
    Cons restValues;
    Cons localNil = Nil.nil;	// gives significant speedup, sadly enough
    for (restNames = names, restValues = values;
	 restNames != localNil;
	 restNames = ((Cons)restNames).cdr, restValues = (Cons)restValues.cdr) 
      {
        if (restNames instanceof Symbol) {
	  if (restNames == name)
	    return restValues;
	  else
	    break;
	}
	else if (((Cons)restNames).car == name)
	  return restValues.car;
      }
    Environment up = null;	// extra set is necessary because of broken verifiers
    switch (lookupType) {
    case Environment.GET_REGULAR: up = parent; break;
    case Environment.GET_DYNAMIC: up = dynamicParent; break;
    case Environment.GET_LOCAL: up = null;
    }
    if (up != null)
      return up.getBinding(name, lookupType);
    else
      throw new SchemeException(name + " is unbound");
  }

  // this version uses exceptions to handle rest bindings.
  // it seems somewhat faster than the usual one, but it's a little too radical.
//   public Object getBinding(Symbol name, int lookupType) throws SchemeException {
//     Object restNames;
//     Cons restValues;
//     Cons localNil = Nil.nil;	// gives significant speedup, sadly enough
//     for (restNames = names, restValues = values;
// 	 restNames != localNil;
// 	 restNames = ((Cons)restNames).cdr, restValues = (Cons)restValues.cdr) 
//       {
// 	try {
//       	  if (((Cons)restNames).car == name)
// 	    return restValues.car;
// 	}
// 	catch (ClassCastException e) {
// 	  if (restNames instanceof Symbol && 
// 	      restNames == name)
// 	    return restValues;
// 	  else
// 	    break;
// 	}
//       }
//     Environment up = null;	// extra set is necessary because of broken verifiers
//     switch (lookupType) {
//     case Environment.GET_REGULAR: up = parent; break;
//     case Environment.GET_DYNAMIC: up = dynamicParent; break;
//     case Environment.GET_LOCAL: up = null;
//     }
//     if (up != null)
//       return up.getBinding(name, lookupType);
//     else
//       throw new SchemeException(name + " is unbound");
//   }

  public String toString() {
    return "ProcEnvironment[" + getLocalBindings().toString() + "]";
  }

  public void forBindings(Procedure proc) throws SchemeException {
    for (Object restNames = names;
	 restNames != Nil.nil; 
	 restNames = ((Cons)restNames).cdr)
      {
        if (restNames instanceof Symbol) {
	  proc.apply(Environment.top, new Cons(restNames));
	  break;
	}
	else 
	  proc.apply(Environment.top, new Cons(((Cons)restNames).car));
      }
  }

  public TopEnvironment getTopEnvironment() {
    if (dynamicParent == null) {
      if (parent == null) 
	return null;
      else
	return parent.getTopEnvironment(); // if stuck, try going up lexical environment
    }
    else
      return dynamicParent.getTopEnvironment();
  }
}
