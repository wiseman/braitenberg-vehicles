package com.ibm.jikes.skij;

/* This file is part of Skij.
 * Author: Michael Travers (mt@watson.ibm.com)
 * 
 * Licensed Materials - See the file license.txt.
 * (c) Copyright IBM Corp. 1997, 1998. All rights reserved.
 */

public class ListenerWindow {

  /* this class exists solely so that application code can do:
   *  Class.forName("com.ibm.jikes.skij.ListenerWindow").newInstance();
   */
  
  public ListenerWindow () {
    Scheme.start("(make-awt-listener-window)");
  }
}
