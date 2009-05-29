package com.ibm.jikes.skij; 

/* This file is part of Skij.
 * Author: Michael Travers (mt@watson.ibm.com)
 * 
 * Licensed Materials - See the file license.txt.
 * (c) Copyright IBM Corp. 1997, 1998. All rights reserved.
 */

/**
 * The class for macro procedures. Macros act exactly like <code>CompoundProcedure</code>s;
 * but the evaluator treats them differently.
 *
 * @see com.ibm.jikes.skij.Evaluator
 */
public class Macro extends CompoundProcedure {

  String shortClassName() {return "Macro";}

  public Macro(Environment e, Object a, Cons b) {
    super(e, a ,b); }

  public Macro(Environment e, Symbol n, Object a, Cons b) {
    super(e, n, a, b); }

}
