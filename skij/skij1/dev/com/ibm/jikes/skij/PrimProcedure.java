package com.ibm.jikes.skij; 
import com.ibm.jikes.skij.util.*;
import java.io.*;

/* This file is part of Skij.
 * Author: Michael Travers (mt@watson.ibm.com)
 * 
 * Licensed Materials - See the file license.txt.
 * (c) Copyright IBM Corp. 1997, 1998. All rights reserved.
 */

/** 
 * This abstract class represents primitve procedures (those implemented
 * in Java). The individual primitives are defined as inner classes based
 * on this. Most but not all are defined in this file. Each inner class 
 * defines its own <code>apply</code> method that contains the definition.
 */
public abstract class PrimProcedure extends Procedure {
  
  /* All Skij primitives are extensions to this class. Most but
   * not all are defined here.
   */

  String shortClassName() {return "Primitive";}

  public PrimProcedure() {
  }

  public PrimProcedure(String n) {
    name = Symbol.intern(n); 
    TopEnvironment.addTopBinding(name, this);
  }
  
  public PrimProcedure(Symbol n) {
    name = n; 
    TopEnvironment.addTopBinding(name, this);
  }

  // utility to check for a fixed number of args
  static void checkArgs(PrimProcedure proc, Cons args, int nargs) throws SchemeException {
    if (args.length() != nargs)
      throw new SchemeException(proc + " wanted " + nargs + " args but was passed " + args.length());
  }

  // this is (map car list), not the CL mapcar
  static Cons mapcar(Cons list) {
    if (list == Nil.nil) 
      return Nil.nil;
    else
      return new Cons(list.caar(), mapcar(list.kdr())); }

  static Cons mapcdr(Cons list) {
    if (list == Nil.nil) 
      return Nil.nil;
    else
      return new Cons(list.kdar(), mapcdr(list.kdr())); }

  static InputPort getDefaultInputPort(Environment env) throws SchemeException {
    return env.getTopEnvironment().in;
  }

  static OutputPort getDefaultOutputPort(Environment env) throws SchemeException {
    return env.getTopEnvironment().out;
  }

  static Boolean boxedBoolean(boolean b) {
    if (b)
      return Boolean.TRUE;
    else
      return Boolean.FALSE;
  }

  static boolean schemeEquals(Object a, Object b) {
    if (a == b) return true;
    else if (a.getClass().isArray() &&
	     b.getClass().isArray()) {
      Object[] av = (Object[])a;
      Object[] bv = (Object[])b;
      if (av.length != bv.length) return false;
      for (int i = 0; i != av.length; i++) {
	if (!schemeEquals(av[i],bv[i]))
	  return false;
      }
      return true;
    }
    else
      return a.equals(b);
  }

  // this is a static class initializer, runs when class is loaded
  static {

    // could be defined in scheme now that dotted arg lists work
    new PrimProcedure("map") {
      public Object apply(Environment env, Cons args) throws SchemeException {
	Procedure proc = (Procedure)args.car;

	Cons result = Nil.nil;
	Cons last = null;
	for (Cons arglists = args.kdr(); 
	     arglists.car != Nil.nil ;
	     arglists = this.mapcdr(arglists)) {
	  Cons newcons = new Cons(proc.apply(env, this.mapcar(arglists)));
	  if (last == null)
	    result = newcons;
	  else
	    last.cdr = newcons;
	  last = newcons;
	}
	return result;
      }
    } ;

    new PrimProcedure("for-each") {
      public Object apply(Environment env, Cons args) throws SchemeException {
	Procedure proc = (Procedure)args.car;
	for (Cons arglists = args.kdr();
	     arglists.car != Nil.nil;) {
	  proc.apply(env, this.mapcar(arglists));
	  for (Cons ag = arglists; ag != Nil.nil ; ag = ag.kdr()) {
	    ag.car = ((Cons)ag.car).cdr;
	  }
	}
	return null;
      }
    } ;

    new PrimProcedure("apply") {
      public Object apply(Environment env, Cons args) throws SchemeException {
	Procedure proc = (Procedure)args.car;
	Cons arguments;
	if (args.length() > 2) {
	  arguments = args.kdr();
	  Cons rest;
	  for (rest = args.kdr(); !(rest.kddr() == Nil.nil); rest = (Cons)rest.cdr) {}
	  rest.cdr = rest.cadr();
	}
	else {
	  arguments = (Cons)args.cadr();
	}
	return proc.apply(env, arguments); }} ;

    new PrimProcedure("equal?") {
      public Object apply(Environment env, Cons args) throws SchemeException {
	return this.boxedBoolean(this.schemeEquals(args.car, args.cadr()));
      }};

    new PrimProcedure("+") { 
      public Object apply(Environment env, Cons args) throws SchemeException {
	return this.accumulateNumber(args, '+', 0);
      }};

    new PrimProcedure("*") { 
      public Object apply(Environment env, Cons args) throws SchemeException {
	return this.accumulateNumber(args, '*', 1);
      }};

    new PrimProcedure("-") { 
      public Object apply(Environment env, Cons args) throws SchemeException {
	if (args.cdr == Nil.nil)
	  return this.accumulateNumber(args, '-', 0);
	else {
	  return this.accumulateNumber(args, '-');
	}
      }};

    new PrimProcedure("/") { 
      public Object apply(Environment env, Cons args) throws SchemeException {
	if (args.cdr == Nil.nil)
	  return this.accumulateNumber(args, '/', 1.0);	// force conversion to inexact
	else {
	  return this.accumulateNumber(args, '/');
	}
      }};

    new PrimProcedure("max") { 
      public Object apply(Environment env, Cons args) throws SchemeException {
	return this.accumulateNumber(args, 'M');
      }};

    new PrimProcedure("min") { 
      public Object apply(Environment env, Cons args) throws SchemeException {
	return this.accumulateNumber(args, 'm');
      }};
    
    new PrimProcedure("remainder") { 
      public Object apply(Environment env, Cons args) throws SchemeException {
	return this.accumulateNumber(args, '%');
      }};

    new PrimProcedure("=") { 
      public Object apply(Environment env, Cons args) throws SchemeException {
	return this.accumulateNumber(args, '=');	
      }};

    new PrimProcedure(">") { 
      public Object apply(Environment env, Cons args) throws SchemeException {
	return this.accumulateNumber(args, '>');	
      }};

    new PrimProcedure("<") { 
      public Object apply(Environment env, Cons args) throws SchemeException {
	return this.accumulateNumber(args, '<');	
      }};

    new PrimProcedure(">=") { 
      public Object apply(Environment env, Cons args) throws SchemeException {
	return this.accumulateNumber(args, '.');	
      }};

    new PrimProcedure("<=") { 
      public Object apply(Environment env, Cons args) throws SchemeException {
	return this.accumulateNumber(args, ',');	
      }};

    new PrimProcedure("cos") { 
      public Object apply(Environment env, Cons args) {
	return new Double(Math.cos(((Number)args.car).doubleValue())); }};

    new PrimProcedure("sin") { 
      public Object apply(Environment env, Cons args) {
	return new Double(Math.sin(((Number)args.car).doubleValue())); }};

    new PrimProcedure("tan") { 
      public Object apply(Environment env, Cons args) {
	return new Double(Math.tan(((Number)args.car).doubleValue())); }};

    new PrimProcedure("exp") { 
      public Object apply(Environment env, Cons args) {
	return new Double(Math.exp(((Number)args.car).doubleValue())); }};

    new PrimProcedure("log") { 
      public Object apply(Environment env, Cons args) {
	return new Double(Math.log(((Number)args.car).doubleValue())); }};

    new PrimProcedure("round") { 
      public Object apply(Environment env, Cons args) {
	if (args.car instanceof Integer)
	  return args.car;
	else
	  // spec says round returns an inexact, who am I to argue?
	  return new Double(Math.round(((Number)args.car).floatValue())); }};

    new PrimProcedure("eq?") {
      public Object apply(Environment env, Cons args) {
	return this.boxedBoolean (args.car == args.cadr()); }} ;

    new PrimProcedure("pair?") {
      public Object apply(Environment env, Cons args) {
	return this.boxedBoolean (args.car instanceof Cons &&
				  !(args.car == Nil.nil)); }} ;

    new PrimProcedure("car") {
      public Object apply(Environment env, Cons args) {
	return ((Cons)args.car).car; }} ;

    new PrimProcedure("cdr") {
      public Object apply(Environment env, Cons args) {
	return ((Cons)args.car).cdr; }} ;

    // used often enough to be a primitive
    new PrimProcedure("cadr") {
      public Object apply(Environment env, Cons args) {
	return ((Cons)args.car).cadr(); }} ;

    new PrimProcedure("cons") {
      public Object apply(Environment env, Cons args) throws SchemeException {
	PrimProcedure.checkArgs(this, args, 2);
	return new Cons(args.car, args.cadr()); }} ;

    new PrimProcedure("list") {
      public Object apply(Environment env, Cons args) throws SchemeException {
	return args; }} ;

    new PrimProcedure("null?") {
      public Object apply(Environment env, Cons args) throws SchemeException {
	return this.boxedBoolean(args.car == Nil.nil); }} ;	

    new PrimProcedure("set-car!") {
      public Object apply(Environment env, Cons args) throws SchemeException {
	Cons mycons = (Cons)args.car;
	mycons.car = args.cadr();
	return null; }} ;

    new PrimProcedure("set-cdr!") {
      public Object apply(Environment env, Cons args) throws SchemeException {
	Cons mycons = (Cons)args.car;
	mycons.cdr = args.cadr();
	return null; }} ;

    new PrimProcedure("current-output-port") {
      public Object apply(Environment env, Cons args) throws SchemeException {
	return this.getDefaultOutputPort(env); }} ;
    
    new PrimProcedure("current-input-port") {
      public Object apply(Environment env, Cons args) throws SchemeException {
	return this.getDefaultInputPort(env); }} ;
    
    // takes one or zero args
    new PrimProcedure("read-char") {
      public Object apply(Environment env, Cons args) throws SchemeException {
	try {
	  return new Character((char)((args == Nil.nil ? this.getDefaultInputPort(env) : (InputPort)args.car).readChar())); }
	catch (EOFException e) {
	  return new EOFObject(); }
	catch (IOException e) {
	  throw new SchemeException(e); }
      }} ;

    // takes one or zero args
    new PrimProcedure("peek-char") {
      public Object apply(Environment env, Cons args) throws SchemeException {
	try {
	  return new Character((char)((args == Nil.nil ? this.getDefaultInputPort(env) : (InputPort)args.car).peekChar())); }
	catch (EOFException e) {
	  return new EOFObject(); }
	catch (IOException e) {
	  throw new SchemeException(e); }
      }} ;


    // takes one or two args
    new PrimProcedure("write-char") {
      public Object apply(Environment env, Cons args) throws SchemeException {
	char cha = ((Character)args.car).charValue();
	OutputPort port = (args.cdr == Nil.nil ? this.getDefaultOutputPort(env) : (OutputPort)args.cadr());
	try {
	  port.writeChar(cha); }
	catch (IOException e) {
	  throw new SchemeException(e); }
	return null; }};

    // this is a primitive because it needs to use casting. Another Java bizarrity.
    new PrimProcedure("integer->char") {
      public Object apply(Environment env, Cons args) throws SchemeException {
	int cha = ((Number)args.car).intValue();
	return new Character((char)cha); }} ;
    
    // takes one or two args
    new PrimProcedure("display") {
      public Object apply(Environment env, Cons args) throws SchemeException {
	OutputPort port = (args.cdr == Nil.nil ? this.getDefaultOutputPort(env) : (OutputPort)args.cadr());
	try {
	  port.write(args.car.toString()); }
	catch (IOException e) {
	  throw new SchemeException(e); }
	return args.car; }} ;

    new PrimProcedure("to-string") {
      public Object apply(Environment env, Cons args) throws SchemeException {        
	return args.car.toString();
      }} ;

    // Evil eval
    new PrimProcedure("eval") {
      public Object apply(Environment env, Cons args) throws SchemeException {    
	Environment theEnv = (Environment)((args.cdr == Nil.nil) ? env : args.cadr());
	return Evaluator.eval(args.car, theEnv); }} ;

    new PrimProcedure("read") {
      public Object apply(Environment env, Cons args) throws SchemeException {    
	InputPort port = (args == Nil.nil ? this.getDefaultInputPort(env) : (InputPort)args.car);
	try {
	  return port.read();
	}
	catch (EOFException e) {
	  return new EOFObject(); }
	catch (IOException e) {
	  throw new SchemeException("error during read", e);
	}
      }} ;

    // needed here for bootstrapping
    new PrimProcedure("load") {
      public Object apply(Environment env, Cons args) throws SchemeException {
	InputStream filestream = null;
	try {
	  String filename = (String)args.car;
	  filestream = new FileInputStream(filename);
	  System.out.println("Loading " + filename + "...");
	  (new SchemeListener(filestream)).repl(); 
	}
	catch (FileNotFoundException e) {
	  throw new SchemeException(e); }
	finally {
	  try { 
	    if (filestream != null) filestream.close();
	  }
	  catch (IOException e) {
	    throw new SchemeException(e); }
	}
	return null;}} ;

    // type coercion, for passing to primitives.
    new PrimProcedure("integer") {
      public Object apply(Environment env, Cons args) {
	Object n = args.car;
	if (n instanceof Integer)
	  return n;
	else
	  return new Integer(((Number)args.car).intValue());
      }};


    new PrimProcedure("vector?") {
      public Object apply(Environment env, Cons args) {
	 return this.boxedBoolean(args.car.getClass().isArray());
      }} ;

    // these handle arrays of primitives, but are slower than the more obvious implementation
    new PrimProcedure("vector-ref") {
       public Object apply(Environment env, Cons args) {
	 return java.lang.reflect.Array.get(args.car, ((Integer)args.cadr()).intValue()) ;
       }} ;

     new PrimProcedure("vector-set!") {
       public Object apply(Environment env, Cons args) {
	 java.lang.reflect.Array.set(args.car, ((Integer)args.cadr()).intValue(), args.caddr()) ;
	 return null;
       }} ;

     new PrimProcedure("vector-length") {
       public Object apply(Environment env, Cons args) {
	 return new Integer(java.lang.reflect.Array.getLength(args.car)) ;
       }} ;


     // return the environment
     new PrimProcedure("current-environment")  {
       public Object apply(Environment env, Cons args) throws SchemeException {
	 return env;
       }} ;

     new PrimProcedure("global-environment")  {
       public Object apply(Environment env, Cons args) throws SchemeException {
	 return Environment.top;
       }} ;

     // we turn whatever the arg is into a SchemException
     // alternatively, we could change every damn method to throw Throwable, then
     // we could handle arbitrary exceptions. Too much work for now, Maybe later.
     new PrimProcedure("throw")  {
       public Object apply(Environment env, Cons args) throws SchemeException {
	 if (args.car instanceof Throwable)
	   throw new SchemeException((Throwable)args.car);
	 else
	   throw new SchemeException(args.car.toString());
       }} ;

     new PrimProcedure("%catch") {
       public Object apply(Environment env, Cons args) throws SchemeException {
	 Procedure thunk = (Procedure)args.car;
	 try {
	   return thunk.apply(env, Nil.nil); 
	 }
	 catch (ContinuationException e) {
	   throw e;
	 }
	 catch (Exception e) {
	   return e;
	 }
       }} ;

     new PrimProcedure("call-with-current-continuation") {
       public Object apply(Environment env, Cons args) throws SchemeException {
	 Procedure proc = (Procedure)args.car;
	 Continuation cont = new Continuation();
	 try {
	   return proc.apply(env, Cons.list(cont));
	 }
	 catch (ContinuationException e) {
	   if (e.continuation == cont)
	     return e.result;
	   else
	     throw e;
	 }
       }} ;

     // From r^5s -- without full call/cc, degenerates to unwind-protect
     new PrimProcedure("dynamic-wind") {
       public Object apply(Environment env, Cons args) throws SchemeException {
	 Object result = null;
	 this.checkArgs(this, args, 3);
	 Procedure before = (Procedure)args.car;
	 Procedure thunk = (Procedure)args.cadr();
	 Procedure after = (Procedure)args.caddr();
	 before.apply(env, Nil.nil);
	 try {
	   result = thunk.apply(env, Nil.nil);
	 }
	 finally {
	   after.apply(env, Nil.nil);
	 }
	 return result;
       }} ;

     new PrimProcedure("%dynamic") {
       public Object apply(Environment env, Cons args) throws SchemeException {
	 Symbol name = (Symbol)args.car;
	 return env.getDynamicBinding(name);
       }} ;

     new PrimProcedure("class-of") {
       public Object apply(Environment env, Cons args) throws SchemeException {
	 return args.car.getClass();
       }} ;

     new PrimProcedure("%instanceof") {
       public Object apply(Environment env, Cons args) throws SchemeException {
	 Class klass = (Class)args.cadr();
	 return this.boxedBoolean(klass.isInstance(args.car));
       }} ;

     // create a java object
     new PrimProcedure("new") {
       public Object apply(Environment env, Cons args) throws SchemeException {
	 try {
	   return Dynvoke.create(this.classArg(args.car), this.makeArray(args.kdr())); }
	 catch (Throwable e) {	// let's be lazy
	   throw new SchemeException("Failed to create instance of " + args.car +
				     " with args " + args.kdr(), e); }
       }};

     // invoke a method on a java object
     new PrimProcedure("invoke") {
       public Object apply(Environment env, Cons args) throws SchemeException {
	 Object obj = args.car;
	 String methodName = args.cadr().toString();
	 Object[] aargs = this.makeArray(args.kddr());
	 try {
	   return Dynvoke.invoke(obj.getClass(), obj, methodName, aargs); 
	 }
	 catch (Throwable e) {
	   throw new SchemeException("Error invoking method " + methodName + " on " + obj + " with args " + args.kddr(), e); }
       }} ;

     new PrimProcedure("invoke-static") {
       public Object apply(Environment env, Cons args) throws SchemeException {
	 String methodName = args.cadr().toString();
	 Object[] aargs = this.makeArray(args.kddr());
	 try {
	   Class myClass = this.classArg(args.car);
	   return Dynvoke.invoke(myClass, null, methodName, aargs); 
	 }
	 // +++ Tue Feb 09 17:50:46 1999 try this; it might let errors in require files not get oerly encapsulated
	 //	 catch (SchemeException e) {
	 //	   throw e;
	 catch (Throwable e) {
	   throw new SchemeException("Error invoking static method " + 
				     args.car + "." + methodName +
				     " with args " + args.kddr(), e); }
       }} ;

     // true if arg is Java null pointer
     new PrimProcedure("%%null?") {
       public Object apply(Environment env, Cons args) throws SchemeException {
	 return this.boxedBoolean(args.car == null);
       }};

     // return java null
     new PrimProcedure("%null") {
       public Object apply(Environment env, Cons args) throws SchemeException {
	 return null;
       }} ;


     // get an instance var
     new PrimProcedure("peek") {
       public Object apply(Environment env, Cons args) throws SchemeException {
	 Object obj = args.car;
	 String fieldName = args.cadr().toString();
	 try {
	   return Dynvoke.peek(obj.getClass(), obj, fieldName); }
	 catch (Throwable e) {
	   throw new SchemeException("Error in peek of " + obj + "." + fieldName, e); }
       }} ;


     new PrimProcedure("peek-static") {
       public Object apply(Environment env, Cons args) throws SchemeException {
	 String fieldName = args.cadr().toString();
	 try {
	   Class myClass = this.classArg(args.car);
	   return Dynvoke.peek(myClass, null, fieldName); }
	 catch (Throwable e) {
	   throw new SchemeException("Error in peek-static of " + args.car + "." + fieldName, e); }
       }} ;


     // set an instance var
     new PrimProcedure("poke") {
       public Object apply(Environment env, Cons args) throws SchemeException {
	 Object obj = args.car;
	 String fieldName = args.cadr().toString();
	 try {
	   Dynvoke.poke(obj.getClass(), obj, fieldName, args.caddr()); 
	   return null;
	 }
	 catch (Throwable e) {
	   throw new SchemeException("Error in poke of " + obj + "." + fieldName, e); }
       }} ;

     new PrimProcedure("poke-static") {
       public Object apply(Environment env, Cons args) throws SchemeException {
	 String fieldName = args.cadr().toString();
	 try {
	   Class myClass = this.classArg(args.car);
	   Dynvoke.poke(myClass, null, fieldName, args.caddr()); 
	   return null;
	 }
	 catch (Throwable e) {
	   throw new SchemeException("Error in poke-static of " + args.car + "." + fieldName, e); }
       }} ;


     new PrimProcedure("%synchronized") {
       public Object apply(Environment env, Cons args) throws SchemeException {
	 Object lockObj = args.car;
	 Procedure thunk = (Procedure)args.cadr();
	 synchronized (lockObj) {
	   return thunk.apply(env, Nil.nil); }
       }} ;

     // was in scheme, but doing it as a primitive makes more things accessible
     new PrimProcedure("class-named") {
       public Object apply(Environment env, Cons args) throws SchemeException {
	 try {
	   return Class.forName(args.car.toString());
	 }
	 catch (ClassNotFoundException e) {
	   throw new SchemeException("Error in class-named", e); }
       }} ;

   } // terminates class static initializer

  /**
   * Turn a list into a 1d Object array.
   * This is called by the Scheme function list->vector, as well as by the invoke primitives.
   */
  public static Object[] makeArray(Cons in) {
    Object[] values = new Object[in.length()];
    Cons nil = Nil.nil;
    int i = 0;
    for (Cons rest = in;
 	 rest != nil;
 	 rest = (Cons)rest.cdr, i = i + 1)
      values[i] = rest.car;
    return values; }

  static Class classArg(Object arg) throws ClassNotFoundException {
    if (arg instanceof Class)
      return (Class)arg;
    else if (arg instanceof String)
      return Class.forName((String)arg);
    else
      return Class.forName(arg.toString());
  }
    

  // utils for n-arg operators

  static Object accumulateNumber(Cons args, char op) {
    return accumulateNumber(args.kdr(), op, (Number)args.car);
  }

  static Object accumulateNumber(Cons args, char op, Number base) {
    if (base instanceof Integer)
      return accumulateNumber(args, op, base.intValue());
    else
      return accumulateNumber(args, op, base.doubleValue());
  }

  static Object accumulateNumber(Cons args, char op, int base) {
    int acc = base;
    int arg1;
    Cons nil = Nil.nil;
    for (Cons rest = args;
	 rest != nil;
	 rest = (Cons)rest.cdr) {
      if (rest.car instanceof java.lang.Integer) {
	arg1 = ((Integer)rest.car).intValue();
	switch (op) {
	case '+': acc += arg1; break;
	case '-': acc -= arg1; break;
	case '*': acc *= arg1; break;
	case '/': acc /= arg1; break;
	case '%': acc %= arg1; break;
	case 'M': if (arg1 > acc) acc = arg1; break; // max
	case 'm': if (arg1 < acc) acc = arg1; break; // min
	  // comparisions
	case '=': if (arg1 == acc) acc = arg1; else return Boolean.FALSE; break;
	case '<': if (acc < arg1) acc = arg1; else return Boolean.FALSE; break;
	case '>': if (acc > arg1) acc = arg1; else return Boolean.FALSE; break;
	case ',': if (acc <= arg1) acc = arg1; else return Boolean.FALSE; break; 
	case '.': if (acc >= arg1) acc = arg1; else return Boolean.FALSE; break;
	default: throw new Error("this shouldn't happen - op: " + op);
	}
      }
      else
	return accumulateNumber(rest, op, (double)acc);
    }
    switch (op) {
    case '+': case '-': case '*': case '/': case '%': case 'M': case 'm': 
      return new Integer(acc);	  
    case '=': case '<': case '>': case ',': case '.': default:
      return Boolean.TRUE;
    }
  }

  // exactly same as above except with doubles
  static Object accumulateNumber(Cons args, char op, double base) {
    double acc = base;
    double arg1;
    Cons nil = Nil.nil;
    for (Cons rest = args;
	 rest != nil;
	 rest = (Cons)rest.cdr) {
      arg1 = ((Number)rest.car).doubleValue();
      switch (op) {
      case '+': acc += arg1; break;
      case '-': acc -= arg1; break;
      case '*': acc *= arg1; break;
      case '/': acc /= arg1; break;
      case '%': acc %= arg1; break;
      case 'M': if (arg1 > acc) acc = arg1; break;
      case 'm': if (arg1 < acc) acc = arg1; break;
	// comparisions
      case '=': if (arg1 == acc) acc = arg1; else return Boolean.FALSE; break;
      case '<': if (acc < arg1) acc = arg1; else return Boolean.FALSE; break;
      case '>': if (acc > arg1) acc = arg1; else return Boolean.FALSE; break;
      case ',': if (acc <= arg1) acc = arg1; else return Boolean.FALSE; break; 
      case '.': if (acc >= arg1) acc = arg1; else return Boolean.FALSE; break;
      default: throw new Error("this shouldn't happen - op: " + op);
      }
    }
    switch (op) {
    case '+': case '-': case '*': case '/': case '%': case 'M': case 'm': 
      return new Double(acc);
    case '=': case '<': case '>': case ',': case '.': default:
      return Boolean.TRUE;
    }
  }
  
}
	 
	   
    
      
      

      

      
