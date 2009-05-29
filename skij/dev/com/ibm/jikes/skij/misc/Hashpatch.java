package com.ibm.jikes.skij.misc;
import com.ibm.jikes.skij.*;

/* This file is part of Skij.
 * Author: Michael Travers (mt@watson.ibm.com)
 * 
 * Licensed Materials - See the file license.txt.
 * (c) Copyright IBM Corp. 1997, 1998. All rights reserved.
 */

/** 
 * Some functions can't be implemented in Scheme due to security problems.
 * The problem is that some public methods return objects that are from non-public
 * classes, and Skij can't access them via reflection (in JDK 1.1).
 * So, these functions which would normally be in Scheme must be written in Java.
 *
 * I'm also here showing how you can define a primitive outside of PrimProcedure.java.
 * This class gets loaded from lib/java.scm, and will overwrite some of its definitions.
 */
public class Hashpatch {

  static {

    (new PrimProcedure("map-enumeration") {
      public Object apply(Environment env, Cons args) throws SchemeException {
	Procedure proc = (Procedure)args.car;
	java.util.Enumeration e = (java.util.Enumeration)args.cadr();
	Object result = Nil.nil;
	Cons last = null;	
	for (; e.hasMoreElements(); ) {
	  Cons newcons = new Cons(proc.apply(env, new Cons(e.nextElement())));
	  if (last == null) 
	    result = newcons;
	  else 
	    last.cdr = newcons;	// tack on to end of list
	  last = newcons;
	}
	return result;
      }}).define(Environment.top);

    (new PrimProcedure("for-enumeration") {
      public Object apply(Environment env, Cons args) throws SchemeException {
	Procedure proc = (Procedure)args.car;
	java.util.Enumeration e = (java.util.Enumeration)args.cadr();
	for (; e.hasMoreElements(); ) 
	  proc.apply(env, new Cons(e.nextElement()));
	return null;
      }}).define(Environment.top);


  }
}
