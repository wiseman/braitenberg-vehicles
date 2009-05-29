package com.ibm.jikes.skij;

/* This file is part of Skij.
 * Author: Michael Travers (mt@watson.ibm.com)
 * 
 * Licensed Materials - See the file license.txt.
 * (c) Copyright IBM Corp. 1997, 1998. All rights reserved.
 */

class Continuation extends Procedure {

  String shortClassName() {return "Continuation";}

  public Object apply(Environment within, Cons args) throws SchemeException {
    throw new ContinuationException(this, args.car);
  }
}
  
