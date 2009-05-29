package com.ibm.jikes.skij;  
import java.io.*;

/* This file is part of Skij.
 * Author: Michael Travers (mt@watson.ibm.com)
 * 
 * Licensed Materials - See the file license.txt.
 * (c) Copyright IBM Corp. 1997, 1998. All rights reserved.
 */

/**
 * Implements a Scheme reader, not entirely satisfactorily.
 */
public class SchemeTokenizer extends StreamTokenizer {

  Reader reader;

  /**
   * Control the action of the readtime eval macro (#,). Can be 'Y' to 
   * allow readtime eval, 'N' to disallow it, or 'W' (for "wrap") to wrap
   * the quoted expression.
   */
  public char readEval = 'Y';		// Yes, No, or Wrap

  public SchemeTokenizer(Reader r) {

    super(r);
    reader = r;

    setSchemeSyntax();
  }

  /**
   * Make a  one-shot tokenizer that takes its input from a string.
   */
  public SchemeTokenizer(String s) {

    this( new StringReader(s));
  }

  static final Object CLOSEPAREN = new Integer(-20); // sigh

  void setSchemeSyntax() {

    resetSyntax();
    // changes from resetSyntax 
    wordChars('a', 'z');
    wordChars('A', 'Z');
    wordChars(128 + 32, 255);
    whitespaceChars(0, ' ');
    quoteChar('"');
    quoteChar('\'');
    // parseNumbers();
    wordChars('0', '9');

    // newlines are whitespace!
    whitespaceChars('\n','\n');

    // changes to default
    wordChars('*','+');		
    wordChars('!','!');		
    wordChars('<','?');		// <=>?
    wordChars('-','/');		// -./
    ordinaryChar('\'');		// singlequote as token
    wordChars(':',':');		// colon is text (for filenames)
    wordChars('\\','\\');	// backslash is text (for filenames)
    wordChars('$','&');		// $%&
    wordChars('~','~');		// ~
    wordChars('^','_');		// ^_
    quoteChar('"');
    commentChar(';');
    ordinaryChar('@');
  }

  /**
   * Read and return a Scheme object. Will be called recursively for lists, etc.
   */
  public Object nextThing() throws IOException, SchemeException {
    return nextThing(false);
  }

  Object nextThing(boolean inList) throws IOException, SchemeException {
    nextToken();
    switch (ttype) {
    case TT_EOF: {
      throw new EOFException("EOF"); }
    case TT_NUMBER: {
      return new Double(nval); } // sval is no good here, would like to look for decimal point
    case TT_WORD: {
      return parseWord(); }
    case '(': {
      try {
	return readList(); }
      catch (EOFException e) {
	throw new SchemeException("EOF in middle of list");
      }
    }
    case ')': {
      if (inList)
	return CLOSEPAREN; 
      else
	throw new SchemeException("Extra close paren seen");
    }
    case '\'': {
      return Cons.list(Symbol.QUOTE, nextThing()); }
    case '"': {
      return sval; }		
    case '#': {
      return readSharped(); }
    case '`': {
      return Cons.list(Symbol.QUASIQUOTE, nextThing()); }
    case '@': {			// designed for ,@, harmless otherwise I suppose
      return "@"; }
    case ',': {
      Object next = nextThing();
      if (next == "@")
	return Cons.list(Symbol.UNQUOTE_SPLICING, nextThing());
      else
	return Cons.list(Symbol.UNQUOTE, next); }
    default: {
      throw new IOException("Unrecognized token " + ttype); }
    }
  }

  Object readList() throws IOException, SchemeException {
    Object next = nextThing(true);
    if (next == CLOSEPAREN)
      return Nil.nil;
    else 
      if (next == Symbol.intern(".")) {
	Object cdr = nextThing();
	Object terminator = nextThing(true);
	if (terminator == CLOSEPAREN)
	  return cdr;
	else
	  throw new IOException("bad dotted list: " + cdr + terminator);
      }
      else
	return new Cons(next, readList());
  }

  char readChar() throws IOException {
    resetSyntax();
    char result = (char)nextToken();
    setSchemeSyntax();
    return result; }

  // read until whitespace and return as string, no syntax

  Object readSharped () throws IOException, SchemeException {
    char firstChar = readChar();
    switch (firstChar) {
    case 't': {return Boolean.TRUE;}
    case 'f': {return Boolean.FALSE;}
      // exact, return an integer instead of our default double
    case 'e': {
      nextToken();
    }
    case '\\': {
      // +++ doesn't handle #\Newline, etc.
      return new Character(readChar());
    }
    case '(': {
      return Scheme.procedure("list->vector").apply(Environment.top, Cons.list(readList()));
    }
    // experimental: act like CL #.
    case ',': {
      switch (readEval) {
      case 'Y': {
	try {
	  return Scheme.eval(nextThing()); }
	catch (SchemeException e) {
	  throw new IOException("error in readtime eval: " + e.toString()); }
      }
      case 'N': {
	throw new SchemeException("readEval is turned off");
      }
      case 'W': {
	return Cons.list(Symbol.intern("%%read-eval"), nextThing());
      }
      }
    }
    default: {
	throw new IOException("don't understand #" + String.valueOf(firstChar));}
    }
  }

  // look at sval, and return a symbol, Int, or Double as appropriate
  Object parseWord() {
    double denom = 0;
    boolean negatedp = false;
    double value = 0;
    boolean numberp = false;

    for (int i = 0; i != sval.length() ; i++) { 
      int cha = (int)sval.charAt(i);
      if (i == 0 && (cha == '+' || cha == '-')) {
	if (cha == '-') negatedp = true; }
      else 
	if (cha >= '0' && cha <= '9') {
	  numberp = true;
	  if (denom == 0)
	    value = value * 10 + (cha - '0');
	  else {
	    // i suspect this is a lousy way to build floats
	    value = value + (cha - '0') / denom;
	    denom = denom * 10; }
	}
	else
	  if (cha == '.')      
	    denom = 10; 
	  else
	    return Symbol.intern(sval);  // seen non-numeric character
    }
    if (numberp) {
      if (negatedp) value = -1 * value;
      if (denom == 0)
	return new Integer((int)value);
      else
	return new Double(value); }
    else
      return Symbol.intern(sval); // wind up here for single chars + - and .  Note that this is wrong for .
  }

}

    
