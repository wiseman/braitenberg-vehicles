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
 * This class is used to start up Scheme as a standalone Java application. Use
 * <blockquote>
 * <code>java com.ibm.jikes.skij.Scheme [init options]</code></blockquote>
 * from a command line. The <code>start</code> methods provide a number of alternate
 * ways to start Skij.
 */

public class Scheme {

  public final static String version = "1.7.3";
  
  public static boolean initedp = false;
  public static PrintWriter out = new PrintWriter(System.out, true);
  public static boolean quiet = false;

  /**
   * Start Scheme by instantiating the Scheme class. It is usually
   * not necessary to do this.
   */
  public Scheme() {
    start();
  }

  static boolean greetAndInitialize(PrintWriter out) {
    try {
      if (Dynvoke.java11) {
	if (!quiet) {
	  out.println("Welcome to Skij (Scheme in Java) version " + version);
	  out.println("by Michael Travers, IBM Research. Feedback to skijusrs@us.ibm.com.");
	  //	  out.println("Running under Java version " + System.getProperty("java.version") + " from " + System.getProperty("java.vendor"));
 	}
 	initLibraries();
	if (!quiet) out.println("Initialized.");
	return true;
      } 
    
      else { 
	out.println("Running under Java version " + System.getProperty("java.version") + " from " + System.getProperty("java.vendor")); 
	out.println("Sorry, you need at least Java 1.1 to run Skij"); 
	return false; 
      } 
    }
    catch (Throwable e) { 
      out.println("Error trying to verify version" + e.toString()); 
      return true; 
    } 
  } 

  /** If Scheme was started with an initialization form, this variable will contain it.  
   */
  public static String initForm = null;

  public static void main(String[] args) {

    for (int i = 0; i < args.length; i++) {
      String arg = args[i];
      if (arg.equals("-help")) {
	System.out.println("-init <form>    eval form instead of starting listener");
	System.out.println("-trace          turn tracing on as in (trace #t), but takes effect early");
	System.out.println("-quiet          don't print greeting and other messages");
	System.out.println("-window         start up in a window rather than console");
	System.exit(0); }
      if (arg.equals("-init")) 
	// kludge: some command lines don't allow spaces, so permit underscore substitute
	initForm = args[++i].replace('_',' '); 
      if (arg.equals("-trace")) 
	Tracer.setOn(true);
      if (arg.equals("-nolibs"))
	javaLibraries = true;
      if (arg.equals("-quiet"))
	quiet = true;
      if (arg.equals("-window"))
	if (initForm == null) 
	  initForm = "(make-awt-listener-window)";
	else
	  throw new Error("can't specify both -window and -init");
    }
    start(initForm, false);
  }

  /** 
   * Start up Skij in the default way. 
   */
  public static void start() {
    start(null, false);
  }

  /** 
   * Start up Skij with an initialization form.
   */
  public static void start(String initString) {
    initForm = initString;
    start(initForm, false);
  }
  
  /** 
   * Start up Skij with an initialization form and in its own thread.
   */
  public static void start(String initString, boolean threadp) {
    
    initForm = initString;

    if (!greetAndInitialize(out))
      System.exit(-1);

    // process an initialization form, or start up listener

    if (initForm == null)
      if (threadp) {
	new Thread(new SchemeListener()).start(); }
      else
	new SchemeListener().run();
    else 			// initForm supplied
      if (threadp) {
	new Thread( new SchemeListener() {
	  public void run() {
	    Scheme.evalPrint(initForm);
	  }}).start();
      }      else
	evalPrint(initForm);
  }

  /**
   * Initialize Scheme without starting any evaluation or REPL loops.
   */
  public static void initLibraries() {
    if (!initedp) {
      Environment.initialize();      // set up global environment
      try {
	loadResource("lib/init.scm");
	initedp = true;
      }
      catch (Throwable e) {
	System.out.println("Can't find init file: " + e.toString());
      }
    }
  }

  /** 
   * Evaluate a string and return the result as an object. 
   */
  public static Object evalString(String string) throws SchemeException {
    initLibraries();
    Object form;
    try {
      form = new SchemeTokenizer(string).nextThing();
    }
    catch (IOException e) {
      throw new SchemeException("error reading string: " + string, e);
    }
    return eval(form);
  }

  /** 
   * Evaluate a string and print the result.
   */
  public static Object evalPrint(String string) {
    try {
      Object value = evalString(string);
      if (value != null && !quiet) 
	System.out.println("Eval returned: " + value.toString());
      return value;
    }
    catch (SchemeException e) {
      System.out.println("Error reading string: " + string + ": " + e.toString());
      return null;
    }
  }

  /** 
   * Evaluate an object in the top-level environment.
   *
   * @exception SchemeException Can throw.
   */
  public static Object eval(Object thing) throws SchemeException {
    return Evaluator.eval(thing, Environment.top);
  }

  /** 
   * Inspect an object.
   */
  public static void inspect(Object thing) {
    initLibraries();
    Scheme.procedure("inspect").safeApply(Cons.list(thing));
  }

  /** 
   * Give a name, return its top-level binding. This method is usually used by
   * Java methods that want to call a Scheme procedure,
   */
  public static Procedure procedure(String name) {
    Procedure proc;
    try {
      return (Procedure)Environment.top.getBinding(Symbol.intern(name));
    }
    catch (Throwable e) {
      System.out.println("Couldn't get procedure " + name + ": " + e.toString());
      return null;
    }
  }

  static boolean javaLibraries = false;

  public static void loadResource(String name) throws SchemeException, ClassNotFoundException, IOException {
    loadResource(name, "com.ibm.jikes.skij.Scheme");  	// any skij class will do
  }

  // called reflectively from init.scm
  public static void loadResource(String name, String from) throws SchemeException, ClassNotFoundException, IOException {
    if (javaLibraries) {
      try {
	loadResourceJava(name, from);
	return;
      }
      catch (SchemeException e) { }
    }
    Class skijClass = Class.forName(from);
    InputStream s = skijClass.getResourceAsStream(name);
    if (s == null)
      throw new SchemeException("resource " + name + " not found");
    else {
      if (!quiet) System.out.println("Loading resource " + name + "...");
      new SchemeListener(s).repl();
      procedure("provide-resource").safeApply(Cons.list(name));
      if (!quiet) System.out.println("...finished loading resource " + name);
    }
  }

  static void loadResourceJava(String name, String from) throws SchemeException {
    // extract lib name (without extension)
    int pos = name.indexOf('.');
    String bname = name.substring(0, pos);
    try {
      Class.forName(from.substring(0, from.lastIndexOf('.')) + "."  + bname.replace('/', '.'));
    }
    catch (Throwable e) {
      throw new SchemeException("Couldn't load library " + name, e);
    }
  }

  public static void loadURL(java.net.URL url) throws IOException, SchemeException {
    new SchemeListener(url.openStream()).repl();
  }
  
}


