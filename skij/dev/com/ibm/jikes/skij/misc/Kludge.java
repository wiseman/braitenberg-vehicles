package com.ibm.jikes.skij.misc;

/* This file is part of Skij.
 * Author: Michael Travers (mt@watson.ibm.com)
 * 
 * Licensed Materials - See the file license.txt.
 * (c) Copyright IBM Corp. 1997, 1998. All rights reserved.
 */

/**
 * Sometimes reflection can't get through various internal classes;
 * this class provides workarounds.  See also Hashpatch.
 */
public class Kludge {

  // images returned by Frame.createImage()
  public static java.awt.Graphics imageGraphics (java.awt.Image image) {
    return image.getGraphics();
  }

  // Processes returned by Runtime.exec()
  public static java.io.OutputStream processOutputStream(java.lang.Process p) {
    return p.getOutputStream();
  }

  public static java.io.InputStream processInputStream(java.lang.Process p) {
    return p.getInputStream();
  }

  public static java.io.InputStream processErrorStream(java.lang.Process p) {
    return p.getErrorStream();
  }

}

    
