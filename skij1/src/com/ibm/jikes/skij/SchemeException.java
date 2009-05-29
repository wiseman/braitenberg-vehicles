
package com.ibm.jikes.skij;  


/* This file is part of Skij.
 * Author: Michael Travers (mt@watson.ibm.com)
 * 
 * Licensed Materials - See the file license.txt.
 * (c) Copyright IBM Corp. 1997, 1998. All rights reserved.
 */

/**
 * All exceptions in Skij.  A SchemeException
 * is created with a message string or another exception to be encapsulated,
 * or both.  SchemeExceptions can accumulate a backtrace (built up as
 * they throw out of nested invocations of <code>eval</code>) which the
 * user can display for debugging.
 */
public class SchemeException extends Exception {

  /** 
   * The last user-visible exception.
   */
  public static SchemeException lastForUser;

  /**
   * The backtrace for this exception, as a list.
   */
  public Cons backtrace = Nil.nil;

  /**
   * The encapsulated exception, if any.
   */
  public Throwable encapsulated;

  public SchemeException() { super(); }
  public SchemeException(String s) { super(s); }
  public SchemeException(Throwable t) {
    super(t.toString());
    encapsulated = t;
  }
  public SchemeException(String s, Throwable t) {
    super(s + ": " + t.toString());
    encapsulated = t;
  }

  void addBacktraceItem(Object item) {
    backtrace = new Cons(item, backtrace);
  }

  /** 
   * Prints a message for the user describing how to see backtraces.
   */
  public void printBacktrace(java.io.PrintWriter out) {
    lastForUser = this;
    if (encapsulated == null) 
      out.println("Type (backtrace) for more information.");
    else
      out.println("Type (backtrace) or (java-backtrace) for more information.");
  }

  /**
   * Return the innermost exception. This can throw information in messages away.
   */
  public Throwable getInner() {
    if (encapsulated == null)
      return this;
    else {
      if (encapsulated instanceof SchemeException) {
	return ((SchemeException)encapsulated).getInner();
      }
      else {
	return encapsulated;
      }
    }
  }

}
