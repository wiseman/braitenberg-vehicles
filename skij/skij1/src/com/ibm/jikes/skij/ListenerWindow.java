package com.ibm.jikes.skij;

/* This file is part of Skij.
 * Author: Michael Travers (mt@watson.ibm.com)
 * 
 * Licensed Materials - See the file license.txt.
 * (c) Copyright IBM Corp. 1997, 1998. All rights reserved.
 */

/** Start up a listener in an AWT window.
 * This class exists solely so that application code can do:
 * <blockquote><code>Class.forName("com.ibm.jikes.skij.WindowConsole").newInstance();</code></blockquote>
 * This allows an application to start up Skij easily without having any
 * nonreflective references to Skij classes (so it can still be compiled
 * without Skij).
 *
 * @see com.ibm.jikes.skij.ListenerConsole
 */
public class ListenerWindow {

  public ListenerWindow () {
    Scheme.start("(make-awt-listener-window)");
  }
}
