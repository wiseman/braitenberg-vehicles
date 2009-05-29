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
 * An extension to Panel that implements double-buffered drawing. Each
 * paint event happens to an offscreen buffer that is then copied to 
 * the panel.
 */
public class DBPanel extends Panel {

  Graphics offGraphics;
  Image offScreen;
  Dimension offScreenSize;

  public DBPanel(Procedure p) {
    super(p);
  }

  public void paint(Graphics g) {
    Dimension d = size();
    if ((offScreen == null) || 
	(d.width != offScreenSize.width) ||
	(d.height != offScreenSize.height)) {
      offScreen = createImage(d.width, d.height);
      offScreenSize = d;
      offGraphics = offScreen.getGraphics();
      offGraphics.setFont(getFont());
    }
    try {
      painter.apply(Environment.top, Cons.list(offGraphics));
    }
    catch (Throwable e) {
      System.out.println("Error painting " + this.toString() + ": " + e.toString());
    }
    g.drawImage(offScreen, 0, 0, null);
  }
}
