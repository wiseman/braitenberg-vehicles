package com.ibm.jikes.skij;
import com.ibm.jikes.skij.util.*;

/* This file is part of Skij.
 * Author: Michael Travers (mt@watson.ibm.com)
 * 
 * Licensed Materials - See the file license.txt.
 * (c) Copyright IBM Corp. 1997, 1998. All rights reserved.
 */

/**
 * This class handles the evaluation of Scheme expressions.
 */
public class Evaluator {
  
  public Environment topEnv;

  /**
   * If true, the original code for macros is saved, so that procedures
   * when printed look like they do when defined. If false (the default),
   * the original code is discarded and replaced with the expansion.
   */
  public static boolean saveMacroSource = false;

  /**
   * If true, tail-call elimination is turned off. This can produce more meaningul
   * backtraces (but can also cause stack overflow).
   */
  public static boolean saveTailCalls = false;

  public Evaluator(SchemeListener l) {
    topEnv = new TopEnvironment(l);
  }

  /**
   * Evaluate <code>exp</code> in the top environment.
   */
  public Object eval(Object exp) throws SchemeException {
    return eval(exp, topEnv);
  }

  /**
   * Evaluate <code>exp</code> in the context of <code>env</code>.
   */
  public static Object eval(Object exp, Environment env) throws SchemeException {
    int level = 1;
    if (Tracer.on)
      level = Tracer.traceIn("eval: " + exp);
    Object result = eval1(exp, env);
    if (Tracer.on)
      Tracer.traceOut(level, "result: " + result);
    return result; }

  static boolean selfEvalp(Object exp) {
    return ((exp instanceof Number) ||
	    (exp instanceof java.lang.String) ||
	    (exp instanceof Boolean) ||
	    (exp instanceof Character)); }

  // false for #f, true for everything else
  static boolean schemeTrue(Object thing) {
    return !(thing instanceof Boolean && 
	     ((Boolean)thing).booleanValue() == false); }

  static Object eval1(Object exp, Environment env) throws SchemeException {
    Procedure proc = null;
    Cons rargs = null;
    Cons nil = Nil.nil;		// speed hack
    try {
    while (true) {
      proc = null;
      rargs = null;
      if (exp instanceof Symbol) {
	return env.getBinding((Symbol)exp); 
      }
      else if (exp instanceof Cons) { // lists
	Cons list = (Cons)exp;
	if (list == nil)
	  throw new SchemeException("Can't eval empty list");
	Object qproc = list.car;
	Cons args = list.kdr();
	//
	if (qproc instanceof Symbol) { // check for special forms
	  String sym = ((Symbol)qproc).name; 
	  if (sym == "begin") {
	    if (args == nil)
	      return null;
	    for (; args.cdr != nil; args = (Cons)args.cdr)
	      eval(args.car, env);
	    exp = args.car;
	  }
	  else if (sym == "quote") 
	    return args.car; 
	  else if (sym == "if") {
	    if (schemeTrue(eval(args.car, env)))
	      exp = args.cadr(); 
	    else
	      if (args.kddr() == nil) // make sure third clause is present
		return null;
	      else
		exp = args.caddr(); 
	  } 
 	  else if (sym == "define") {
	    return doDefine(env, args.car, args.kdr()); }
	  else if (sym == "lambda")
	    return makeProc(env, args.car,args.kdr());
	  else if (sym == "named-lambda")
	    return makeProc(env, args.car, args.cadr(), args.kddr());
	  else if (sym == "set!") {
	    env.setBinding((Symbol)args.car, eval(args.cadr(), env));
	    return null; }
	  else if (sym == "quasiquote") {
	    return evalQuasi(env, args.car); }
	  else if (sym == "unquote" | sym == "unquote-splicing") {
	    throw new SchemeException(", or ,@ seen outside of `"); }
	  // should be replaced by macros
	  else if (sym == "and") {
	    if (args == nil)
	      return Boolean.TRUE;
	    else
	      return evalAnd(args, env); }
	  else if (sym == "or") {
	    if (args == nil)
	      return Boolean.FALSE;
	    else
	      return evalOr(args, env); }
	  else
	    proc = (Procedure)eval(qproc, env);
	} // end car is symbol
	else 
	  proc = (Procedure)eval(qproc, env);
	// here we either are doing a tail-call or we have a procedure
	if (proc != null) {
	  // expand macros in-place
	  if (proc instanceof Macro) {
	    Object expansion = proc.apply(env, list);
	    if (saveMacroSource) {
	      list.car = Symbol.BEGIN;
	      list.cdr = Cons.list(Cons.list(Symbol.QUOTE,
					     Symbol.intern("%%macro-source"),
					     new Cons(list.car,list.cdr)),expansion);
	    }
	    else
	      if (expansion instanceof Cons) {
		list.car = ((Cons)expansion).car;
		list.cdr = ((Cons)expansion).cdr;
	      }
	      else {
		list.car = Symbol.BEGIN;
		list.cdr = Cons.list(expansion);
	      }
	  }

	  // here we do apply of compoundProcedures. To make tail-calling work we need to
	  // violate modularity; this stuff used to be part of the CompoundProcedure class.
	  else if (proc instanceof CompoundProcedure) {
	    CompoundProcedure cproc = (CompoundProcedure)proc;
	    exp =  cproc.body;
	    rargs = mapEval(list.kdr(), env);
	    env = cproc.callEnvironment(env, rargs);
	  }
	  // a primitive procedure
	  else {
	    rargs = mapEval(list.kdr(), env);
	    return proc.apply(env, rargs); } // normal apply
	} // end if (proc != null)
      } // end Cons
      else if (selfEvalp(exp))
	return exp;
      else
	// some weird object we don't know about
	throw new SchemeException("Can't eval " + exp);
      if (Tracer.on)
	Tracer.tracePrint("reduce: " + exp);
      if (saveTailCalls)
	return eval(exp, env);
    } // end while(true)
    } // end try
    catch (ContinuationException e) {
      throw e;			// don't build backtrace for Continuations
    }
    catch (SchemeException e) {
      addBacktraceItem(e, exp, proc, env, rargs);
      throw e;
    }
    catch (Throwable e) {	// turn anything else into a SchemeException
      SchemeException ee = new SchemeException(e);
      addBacktraceItem(ee, exp, proc, env, rargs);
      throw ee;
    }
  }

  static void addBacktraceItem(SchemeException e, Object exp, Procedure proc, Environment env, Cons args) {
    if (args == null || proc == null) 
      e.addBacktraceItem(Cons.list("eval",  exp, "in", env));
    else
      e.addBacktraceItem(Cons.list("apply", proc, "to", args));
  }

  // another case where you want runtime type dispatching on args
  static Object doDefine(Environment env, Object name, Cons definition) throws SchemeException {
    if (name instanceof Cons) {
      Cons nname = (Cons)name;
      // adding a cons that gets stripped immediately, because other case needs it.
      return doDefine(env, nname.car, new Cons(new Cons(Symbol.intern("named-lambda"),
							new Cons(nname.car,
								 new Cons(nname.cdr, definition))))); }
    else {
      Object value =  eval(definition.car, env);
      env.addBinding((Symbol)name, value); 
      return value; }
  }

  static CompoundProcedure makeProc(Environment env, Object args, Cons body){
    return makeProc(env, null, args, body);
  }

  // these can be cached iff body has no free references. But that is hard to determine, esp. with macros
  static CompoundProcedure makeProc(Environment env, Object name, Object args, Cons body){
    return new CompoundProcedure(env, (Symbol)name, args, new Cons(Symbol.BEGIN, body)); 
  }

  static Cons mapEval(Cons in, Environment env) throws SchemeException {
    Cons nil = Nil.nil;		// this produces about a 5% speedup, since Java is too dim to pull the static var lookup out of the loop...
    Cons result = nil;
    Cons finger = result;
    for (Cons rest = in; rest != nil; rest = (Cons)rest.cdr) {
      Cons newCons = new Cons(eval(rest.car, env));
      if (finger == nil) 
	result = newCons; 
      else
	finger.cdr = newCons;
      finger = newCons;
    }
    return result;
  }

  // not usually used
  static Object evalProg(Cons progbody, Environment env) throws SchemeException {
    Object result = null;
    for (Cons rest = progbody; rest != Nil.nil; rest = rest.kdr()) {
      result = eval(rest.car, env);
    }
    return result;
  }

  // the guts of this are in Scheme
  static Object evalQuasi(Environment env, Object exp) throws SchemeException {
    return Scheme.procedure("eval-quasi").apply(env, Cons.list(exp, env)); }

  static Object evalAnd(Cons body, Environment env) throws SchemeException {
    Object temp = eval(body.car, env);
    if (schemeTrue(temp))
      if (body.cdr == Nil.nil)
	return temp;
      else
	return evalAnd(body.kdr(), env);
    else
      return temp;		// always #f
  }
  
  static Object evalOr(Cons body, Environment env) throws SchemeException {
    Object temp = eval(body.car, env);
    if (schemeTrue(temp))
      return temp;
    else
      if (body.cdr == Nil.nil)
	return temp;		// always #f
      else
	return evalOr(body.kdr(), env);
  }
}
