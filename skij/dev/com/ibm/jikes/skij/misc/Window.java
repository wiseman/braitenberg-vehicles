// +++ FLUSH THIS CLASS; use Panel instead

package com.ibm.jikes.skij.misc;
import com.ibm.jikes.skij.*;
import java.awt.*;

/* This file is part of Skij.
 * Author: Michael Travers (mt@watson.ibm.com)
 * 
 * Licensed Materials - See the file license.txt.
 * (c) Copyright IBM Corp. 1997, 1998. All rights reserved.
 */

public class Window extends java.awt.Frame {

  Procedure painter;

  public Window(String name, Procedure p) {
    super(name);
    painter = p;
  }
  
  public void paint(Graphics g) {
    try {
      painter.apply(Environment.top, Cons.list(g));
    }
    catch (SchemeException e) {
      System.out.println("Error painting " + this.toString() + ": " + e.toString());
    }
  }
}
