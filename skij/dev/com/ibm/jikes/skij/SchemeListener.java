package com.ibm.jikes.skij;  
import java.io.*;
import com.ibm.jikes.skij.util.*;  

/* This file is part of Skij.
 * Author: Michael Travers (mt@watson.ibm.com)
 * 
 * Licensed Materials - See the file license.txt.
 * (c) Copyright IBM Corp. 1997, 1998. All rights reserved.
 */

/**
 * Implements a listener (read-eval-print loop).
 */
public class SchemeListener implements Runnable {
  
  public static SchemeListener console;
  boolean prompt = true;

  SchemeTokenizer reader;

  Reader in; 
  PrintWriter out;
  PrintWriter error;

  public SchemeListener() {
    console = this;
    startUp(new InputStreamReader(System.in), Scheme.out, true); }

  public SchemeListener(InputStream i) {
    startUp(new BufferedReader(new InputStreamReader(i)), Scheme.out, false); }

  public SchemeListener(InputStream i, OutputStream o, boolean prompt) {
    startUp(new BufferedReader(new InputStreamReader(i)), new PrintWriter(o, true), prompt); }

  public SchemeListener(Reader i, Writer o, boolean prompt) {
    startUp(i, new PrintWriter(o, prompt), prompt); }

  public SchemeListener(Reader i, PrintWriter o, boolean prompt) {
    startUp(i, o, prompt); }


  void startUp(Reader i, PrintWriter o, boolean p) {
    in = i; out = o;
    error = o;			// no way to differentiate this, for now
    prompt = p;
    reader = new SchemeTokenizer(in);
    evaluator = new Evaluator(this);
  }

  Evaluator evaluator;

  // see lib/listener.scm
  public Environment getTopEnv() {
    return evaluator.topEnv;
  }

  /**
   * Start a read-eval-print loop.
   */
  public void repl() throws SchemeException {
    while (true) {
      Tracer.level = 0;
      try {
	while (true) {
	  doIt(); }
      }
      catch(java.io.EOFException e) {
	if (prompt)
	  error.println(e.toString());
	return;
      }	
      catch (SchemeException e) {
	if (prompt) {
	  error.println("\n" + e.toString()); 
	  e.printBacktrace(out);
	}
	else
	  throw e;
      }
      catch(Throwable e) {
	if (prompt)
	  error.println("\n" + e.toString()); 
	else
	  throw new SchemeException(e);
      }
    }
  }

  // process a single form
  Object doIt() throws java.io.IOException, SchemeException {
    int lev = 0;
    if (prompt) {
      out.print("\nskij> ");
      out.flush(); }
    Object form = reader.nextThing();
    if (Tracer.on)
      lev = Tracer.traceIn("form = " + form);
    Object result = evaluator.eval(form);
    if (prompt && result != null) {
      // this should be per-evaluator really
      Environment.top.addBinding(Symbol.intern("<<<"), result);
      out.print("  <<< "); 
      Scheme.procedure("write").apply(evaluator.topEnv, Cons.list(result));
    }
    if (Tracer.on)
      Tracer.traceOut(lev);
    return result;
  }

   public void run() {
     try {
       repl();
     }
     catch (SchemeException e) {
       error.println("\n" + e.toString()); 
       e.printBacktrace(out);       
     }
   }

}

  
    
      
      
    


       
