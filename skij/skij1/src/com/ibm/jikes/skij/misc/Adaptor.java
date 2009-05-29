package com.ibm.jikes.skij.misc;
import com.ibm.jikes.skij.*;

/* This file is part of Skij.
 * Author: Michael Travers (mt@watson.ibm.com)
 * 
 * Licensed Materials - See the file license.txt.
 * (c) Copyright IBM Corp. 1997, 1998. All rights reserved.
 */

/**
 * This utility class is intended to allow users to package up Skij structure
 * in the guise of a Java object that has its own methods. Those methods can be
 * defined by Skij callbacks. So far, toString is the only method implemented, but
 * it's easy to add more. 
 * 
 * For an example of use, see lib/tree.scm.
 */

public class Adaptor extends ProcEnvironment {

  /**
   * The encapsulated object.
   */
  public Object object;

  public Adaptor(Object o) {
    super();
    object = o;
  }

  /**
   * Calls the procedure that is bound to the symbol <code>toString</code>.
   */
  public String toString() {
    try {
      return (String)invokeProc(Symbol.intern("toString"), Nil.nil);
    }
    catch (SchemeException e) {
      return "[Unprintable Adaptor for " + object.toString();
    }
  }

  Object invokeProc(Symbol name, Cons args) throws SchemeException {
    Procedure proc = (Procedure)getBinding(name);
    if (proc == null) 
      throw new SchemeException("no binding found for " + name);
    return proc.apply(this, args);
  }
    

}
