package com.ibm.jikes.skij;
import java.util.*;

/* This file is part of Skij.
 * Author: Michael Travers (mt@watson.ibm.com)
 * 
 * Licensed Materials - See the file license.txt.
 * (c) Copyright IBM Corp. 1997, 1998. All rights reserved.
 */

/**
 * The class for Scheme symbols.
 */
public class Symbol {

  /**
   * The name of the symbol (as a string).
   */
  public String name;
  int index = -1;	// used by TopEnvironment

  static Hashtable namespace = new Hashtable(800); // somewhat arbitrary number

  // constructor is not public -- use intern instead
  private Symbol(String n) {
    name = n;
  }

  /** 
   * Return a symbol with a given name (possibly creating it). This
   * is the only approved way to create a symbol.
   */
  public static Symbol intern(String n) {
    String nn = n.intern();
    Object found = namespace.get(nn);
    if (found == null) {
      Symbol newsymbol = new Symbol(nn);
      namespace.put(nn, newsymbol);
      return newsymbol;
    }
    else
      return (Symbol)found;
  }

  public String toString() { return name; }

  // symbols frequently referred to from Java
  static final Symbol QUOTE = Symbol.intern("quote");
  static final Symbol QUASIQUOTE = Symbol.intern("quasiquote");
  static final Symbol UNQUOTE = Symbol.intern("unquote");
  static final Symbol UNQUOTE_SPLICING = Symbol.intern("unquote-splicing");
  static final Symbol BEGIN = Symbol.intern("begin");
}




