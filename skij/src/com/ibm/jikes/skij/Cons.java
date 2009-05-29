package com.ibm.jikes.skij;

/* This file is part of Skij.
 * Author: Michael Travers (mt@watson.ibm.com)
 * 
 * Licensed Materials - See the file license.txt.
 * (c) Copyright IBM Corp. 1997, 1998. All rights reserved.
 */

/**
 * A Scheme pair. <code>car</code> and <code>cdr</code> are slots, some other list access functions
 * (ie, <code>cadr</code>) are implemented as methods. Methods starting with <code>k</code> (ie, <code>kdr</code> and
 * <code>kadr</code>) perform an access and cast their result to another Cons. The
 * list methods are convenient for creating lists from within Java code.
 */
public class Cons {

  // NIL is its own class (extended from Cons)

  public Object car = Nil.nil;
  public Object cdr = Nil.nil;	

  public Cons() {}

  public Cons(Object tcar) {
    car = tcar; } 

  public Cons(Object tcar, Object tcdr) {
    car = tcar; cdr = tcdr; }

  static String stringify(Object x) {
    if (x == null) 
      return "#<null>";
    else
      return x.toString();
  }

  public String toString() {
    if (cdr != Nil.nil) {
      if (car == Symbol.QUOTE)
	return "'" + stringify(cadr());
      if (car == Symbol.QUASIQUOTE)
	return "`" + stringify(cadr());
      if (car == Symbol.UNQUOTE)
	return "," + stringify(cadr());
      if (car == Symbol.UNQUOTE_SPLICING)
	return ",@" + stringify(cadr());
    }
    if (cdr instanceof Cons)
      return "(" + car + kdr().toStringList(); 
    else
      return "(" + car + " . " + cdr + ")";	// null?
  }

//   String toStringList() {
//     if (cdr instanceof Cons)
//       return " " + car + kdr().toStringList(); 
//     else
//       return " " + car + " . " + cdr + ")";
//   }


  // iterative implementation (prevents stack overflows)
  String toStringList() {
    String result = "";
    for (Cons me = this;;) {
      if (me == Nil.nil) {
	result += ")";
	break;
      }
      else
	if (me.cdr instanceof Cons) {
	  result += " ";
	  result +=  stringify(me.car);
	  me = me.kdr();
	}
	else {
	  result += " ";
	  result += stringify(me.car);
	  result += " . ";
	  result += stringify(me.cdr);
	  result += ")";
	  break;
	}
    }
    return result;
  }

  public boolean equals(Object obj) {
    return (this == obj) ||
      ((obj instanceof Cons) && 
       (obj != Nil.nil) &&	// Nil has its own equals method
       (PrimProcedure.schemeEquals(this.car, ((Cons)obj).car)) && 
       (PrimProcedure.schemeEquals(this.cdr, ((Cons)obj).cdr)));
  }

  /**
   * Returns the cdr cast to be a pair. Convenient for cdr'ing down a list.
   */
  public Cons kdr() {
    return (Cons)cdr; }

  public Cons kar() {
    return (Cons)car; }

  public Object cadr() {
    return kdr().car; }

  public Cons kadr() {
    return kdr().kar(); }

  public Cons kddr() {
    return kdr().kdr(); }

  public Cons kdar() {
    return kar().kdr(); }

  public Object caar() {
    return kar().car; }

  public Cons kdddr() {
    return kdr().kdr().kdr(); }

  public Object caddr() {
    return kdr().kdr().car; }

  public Object cadddr() {
    return kdr().kdr().kdr().car; }

  /**
   * Return the length of this list as an int.
   */
  public int length() {
    int len = 0;
    for (Cons rest = this;
	 rest != Nil.nil;
	 rest = rest.kdr())
      len = len + 1;
    return len; }

  /**
   * List building methods. These are a convenient way to construct
   * a list from Java code.
   */
  public static Cons list(Object a) {
    return new Cons(a); }

  public static Cons list(Object a, Object b) {
    return new Cons(a, new Cons(b)); }

  public static Cons list(Object a, Object b, Object c) {
    return new Cons(a, new Cons(b, new Cons(c))); }

  public static Cons list(Object a, Object b, Object c, Object d) {
    return new Cons(a, new Cons(b, new Cons(c, new Cons(d)))); }

  public static Cons list(Object a, Object b, Object c, Object d, Object e) {
    return new Cons(a, new Cons(b, new Cons(c, new Cons(d, new Cons(e))))); }
    
}
