package com.ibm.jikes.skij;
 import com.ibm.jikes.skij.util.*;
import java.awt.*;
import java.io.*;
import java.applet.Applet;

/* This file is part of Skij.
 * Author: Michael Travers (mt@watson.ibm.com)
 * 
 * Licensed Materials - See the file license.txt.
 * (c) Copyright IBM Corp. 1997, 1998. All rights reserved.
 */

/**
 * An applet that provides a Skij listener.
 */
public class Skijlet extends Applet  { 

  TextArea textArea;
  
  public void init() {
    super.init();

    //    textArea = new TextArea();
    textArea = new com.ibm.jikes.skij.misc.ListenerTextArea();
    setLayout(new GridLayout(1, 1));
    add(textArea);

    StringWriter w = new StringWriter();

    // use Java libraries (this should maybe be a parameter)
    Scheme.javaLibraries = true;

    if (Scheme.greetAndInitialize(new PrintWriter(w, true))) {
      
      // using appendText rather than append because Netscape (as of 4.05) doesn't
      // support the latter.
      textArea.appendText(w.toString());

      if (Scheme.initedp) {
	Environment.top.addBinding(Symbol.intern("*applet-pane*"), this); // in Swing, these are different
	Environment.top.addBinding(Symbol.intern("*applet*"), this);

	try {

	  String init = getParameter("init");
	  Object form;
	  if (init == null) 
	    form = Cons.list(Symbol.intern("make-applet-listener"), Cons.list(Symbol.intern("quote"), textArea));
	  else 
	    form = new SchemeTokenizer(init).nextThing();
	  Scheme.eval(form);
	}
	catch (Throwable e) {
	  textArea.appendText(e.toString()); }
      }
      else {
	textArea.appendText("couldn't find libraries, so can't run in applet window.\nTrying to run on console...");
	new SchemeListener().run();      
      }
    }
    else {
      // version was off, just show greeting and stop
      textArea.appendText(w.toString());
    }
  }
}
    
    
      
    
      

  
