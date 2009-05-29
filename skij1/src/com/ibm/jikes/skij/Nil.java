package com.ibm.jikes.skij; 
  
/* This file is part of Skij.
 * Author: Michael Travers (mt@watson.ibm.com)
 * 
 * Licensed Materials - See the file license.txt.
 * (c) Copyright IBM Corp. 1997, 1998. All rights reserved.
 */

/**
 * A singleton class that represents the Scheme empty list (<code>nil<code>).
 * Java code should refer to this value as <code>Nil.nil</code>.
 */
public class Nil extends Cons {

  public static final Nil nil = new Nil();

  public String toString() {
    return "()"; }

  String toStringList() {
    return ")"; }

  public boolean equals(Object obj) {
    return obj == Nil.nil;
  }

}
