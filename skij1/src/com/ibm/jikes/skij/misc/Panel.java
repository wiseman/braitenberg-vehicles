package com.ibm.jikes.skij.misc;
import com.ibm.jikes.skij.*;
import java.awt.*;

/* This file is part of Skij.
 * Author: Michael Travers (mt@watson.ibm.com)
 * 
 * Licensed Materials - See the file license.txt.
 * (c) Copyright IBM Corp. 1997, 1998. All rights reserved.
 */

/** 
 * An extension to java.awt.Panel that paints itself using a Scheme
 * procedure. Each instance contains a Scheme procedure of one argument.
 * This procedure is called with the Graphics object as an argument
 * when the panel has to be drawn.
 */
public class Panel extends java.awt.Panel {

  Procedure painter;

  public Panel() {
  }

  public Panel(Procedure p) {
    super();
    painter = p;
  }
  
  public void paint(Graphics g) {
    try {
      painter.apply(Environment.top, Cons.list(g));
    }
    catch (Throwable e) {
      System.out.println("Error painting " + this.toString() + ": " + e.toString());
    }
  }
}
