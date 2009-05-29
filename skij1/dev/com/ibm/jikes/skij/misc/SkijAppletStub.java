package com.ibm.jikes.skij.misc;
import com.ibm.jikes.skij.*;
import java.applet.*;
import java.net.URL;

/* This file is part of Skij.
 * Author: Michael Travers (mt@watson.ibm.com)
 * 
 * Licensed Materials - See the file license.txt.
 * (c) Copyright IBM Corp. 1997, 1998. All rights reserved.
 */

/**
 * Support for starting applets from within skij. See lib/applet.scm.
 */
public class SkijAppletStub implements AppletStub {

  Cons parameters = Nil.nil;
  URL documentBase;
  URL codeBase;

  public SkijAppletStub(Cons params, URL documentB, URL codeB) {
    parameters = params;
    documentBase = documentB;
    codeBase = codeB;
  }

  // the interface methods (placeholders, mostly +++)

  public boolean isActive() {
    return true;
  }

  public URL getDocumentBase() {
    return documentBase;
  }

  public URL getCodeBase() {
    return codeBase;
  }

  public String getParameter(String name) {
    Object term = Scheme.procedure("assoc").safeApply(Cons.list(name, parameters));
    if (term == Boolean.FALSE)
      return null;
    else
      return ((Cons)term).cadr().toString();
  }

  AppletContext context;

  public AppletContext getAppletContext() {
    if (context == null)
      context = new SkijAppletContext();
    return context;
  }

  public void appletResize(int width, int height){
  }

}
