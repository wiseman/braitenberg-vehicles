package com.ibm.jikes.skij;


/* This file is part of Skij.
 * Author: Michael Travers (mt@watson.ibm.com)
 * 
 * Licensed Materials - See the file license.txt.
 * (c) Copyright IBM Corp. 1997, 1998. All rights reserved.
 */

/**
 * This class represents procedures defined in Scheme (nonprimitives).
 */
public class CompoundProcedure extends Procedure implements Runnable {

  /** 
   * The definition (lexical) environment of this procedure.
   */
  public Environment env;

  /**
   * The formal arguments for this procedure. May be a list or a symbol.
   */
  public Object args;		

  /**
   * The body of the procedure; generally a <code>begin</code> form.
   */
  public Cons body;

  String shortClassName() { return "Procedure";}

  CompoundProcedure(Environment e, Object a, Cons b) {
    this(e, null, a, b); }

  CompoundProcedure(Environment e, Symbol n, Object a, Cons b) {
    env = e; args = a; body = b; name = n; }


  /**
   * Apply the procedure to some arguments. This is rarely used; instead
   * the evaluator uses <code>callEnvironment</code>.
   */
  public Object apply(Environment appenv, Cons realargs) throws SchemeException {
    Environment newenv = callEnvironment(appenv, realargs);
    return Evaluator.evalProg(body.kdr(), newenv); 
  }
  
  // create an environment for an apply
  Environment callEnvironment(Environment appenv, Cons realargs) throws SchemeException {

    Environment newenv = new ProcEnvironment(env, appenv, args, realargs); 
    
    Object restargs;
    Cons restreals;

    Cons nil = Nil.nil;
    for(restargs = args, restreals = realargs;
	restargs != nil;
	restargs = ((Cons)restargs).cdr, restreals = restreals.kdr()) {

      if (restargs instanceof Cons) {
	if (restreals == nil)
	  throw new SchemeException("not enough arguments to " + this);
      }
      else {			// this is the non-nil-tailcdr case
	restreals = nil; 
	break;
      } 
    }
    
    if (restreals != nil) 
      throw new SchemeException("too many arguments to " + this);

    return newenv;
  }
    
  /**
   * Use the procedure as root of a thread.
   */
  public void run() {
    try {
      if (args != Nil.nil) 
	throw new SchemeException("can't run " + this + ", it's not a thunk");
      apply(Environment.top, Nil.nil); }  
    catch (SchemeException e) {
      System.out.println("Thunk " + this + " threw " + e); 
      e.printBacktrace(Scheme.out);
    }
  }

}

