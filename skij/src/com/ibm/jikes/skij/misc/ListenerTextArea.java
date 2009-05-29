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
 * This class exists so that listeners can work in browsers
 * that don't support 1.1 events. Argh.
 */
public class ListenerTextArea extends TextArea {

  public Procedure handler;
  
  public boolean keyDown(Event evt, int key) {
    super.keyDown(evt, key);
    try {
      Object result = handler.apply(Environment.top, Cons.list(evt, new Integer(key)));

      if (result == Boolean.FALSE)
	return false;
      else
	return true;
    }
    catch (SchemeException e) {
      Scheme.out.println("Error in ListenerTextArea handler: " + e);
      e.printBacktrace(Scheme.out);
      return false;
   }
  }
}
