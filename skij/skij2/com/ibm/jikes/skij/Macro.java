package com.ibm.jikes.skij; 

/* This file is part of Skij.
 * Author: Michael Travers (mt@watson.ibm.com)
 * 
 * Licensed Materials - See the file license.txt.
 * (c) Copyright IBM Corp. 1997, 1998. All rights reserved.
 */

public class Macro extends CompoundProcedure implements FExpr {

  // macros are just procedures; the evaluator treats them differently
  
  String shortClassName() {return "Macro";}

  public Macro(Environment e, Object a, Cons b) {
    super(e, a ,b); }

  public Macro(Environment e, Symbol n, Object a, Cons b) {
    super(e, n, a, b); }

}
