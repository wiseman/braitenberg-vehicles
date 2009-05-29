package com.ibm.jikes.skij.util;

/* This file is part of Skij.
 * Author: Michael Travers (mt@watson.ibm.com)
 * 
 * Licensed Materials - See the file license.txt.
 * (c) Copyright IBM Corp. 1997, 1998. All rights reserved.
 */

  /** 
   * The Tracer class provides a hierarchical debugging trace. Programs
   * can insert tracing calls into code.  The tracer has a notion of 
   * a current <code>level</code> which indicates how far to indent messages.
   * <p>
   * There are three basic trace-generating
   * methods: <code>tracePrint</code> prints a string at the current level,
   * <code>traceIn</code> prints a message and moves in one level, while <code>traceOut</code> moves
   * out one or more levels before printing a message. The idea is that
   * you can surround a body of code with a <code>traceIn</code>/<code>traceOut</code> pair, and any
   * tracing messages produced during the execution of that code will be 
   * suitably indented.
   *
   * <code>traceIn</code> returns an integer value representing the new level. This number
   * may be passed to <code>traceOut</code> in order to back up to the appropriate level.
   * This is useful in the case of exceptions which might bypass some <code>traceOut</code>s.
   */
public class Tracer {

  
  /**
   * true iff tracing is on.
   */
  public static boolean on = false;

  /**
   * The current level. Zero is top level, higher levels are indented more.
   */
  public static int level = 0;

  /**
   * Turn tracing on or off.
   *
   * @param newOn true to turn tracing on, false to turn it off.
   */
  public static void setOn(boolean newOn) {
    on = newOn; }

  /**
   * Print a message at the current level.
   *
   * @param msg The string to print.
   */
  public static void tracePrint(String msg) {
    if (on) {
      System.out.println();
      for (int i = 0; i != level; i = i+1) 
	System.out.print("  "); 
      System.out.print(msg);
    }
  }

  /**
   * Go in one level, printing a message.
   *
   * @param msg The string to print.
   * @result the new level
   */
  public static int traceIn(String msg) {
    tracePrint(msg);
    return level = level + 1;
  }
  
  /**
   * Go out one level.
   */
  public static void traceOut() {
    setLevel(level - 1); }

  /**
   * Go out one level, printing a message.
   *
   * @param msg The string to print.
   * @result the new level
   */
  public static void traceOut(String msg) {
    setLevel(level - 1); 
    tracePrint(msg);
  }

  /**
   * Go out to a given level.
   *
   */
  public static void traceOut(int lev) {
    setLevel(lev - 1); }

  /**
   * Go out to a given level, printing a message.
   *
   * @param msg The string to print.
   */
  public static void traceOut(int lev, String msg) {
    setLevel(lev - 1); 
    tracePrint(msg);
  }

  // set a level, making sure we don't go negative.
  static void setLevel(int nlevel) {
    level = nlevel < 0 ? 0 : nlevel;
  }

  // This code will demo tracing
//   public static void main(String argv[]) {
//     System.out.println("fact(10)=" + fact(10));
//   }

//   static int fact(int n) {
//     traceIn("enter fact " + n);
//     if (n == 0)
//       {traceOut("return 1"); return 1;}
//     else {
//       int result = n * fact(n - 1);
//       traceOut("return " + result); return result; }
//   }
}
