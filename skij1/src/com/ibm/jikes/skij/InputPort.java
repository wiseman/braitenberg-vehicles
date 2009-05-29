package com.ibm.jikes.skij;
import java.io.*;

/* This file is part of Skij.
 * Author: Michael Travers (mt@watson.ibm.com)
 * 
 * Licensed Materials - See the file license.txt.
 * (c) Copyright IBM Corp. 1997, 1998. All rights reserved.
 */

/**
 * This implements Scheme input ports by acting as a wrapper around a Java 
 * reader.
 * <p>
 * The implementation of <code>read</code> (Lisp expression reading) has a
 * few problems. In particular,  mixed reads and read-chars won't work
 * (chars will get lost in the tokenizer buffer).
 */
public class InputPort {
  
  public Reader reader;
  SchemeTokenizer tokenizer;

  public InputPort(InputStream s) {
    reader = new InputStreamReader(s); }

  public InputPort(Reader r) {
    reader = r;
  }

  /**
   * Read a single character.
   */
  public int readChar() throws java.io.IOException {
    int ch = reader.read(); 
    if (ch == -1)
      throw new java.io.EOFException();
    else
      return ch;
  }

  /**
   * Read a single character, leaving it in the buffer.
   */
  public int peekChar() throws java.io.IOException {
    reader.mark(1);
    int cha = reader.read();
    reader.reset();
    return cha;
  }

  /**
   * Read a Scheme expression.
   */
  public Object read() throws java.io.IOException, SchemeException {
    if (tokenizer == null) 
      tokenizer = new SchemeTokenizer(reader);
    return tokenizer.nextThing();
  }

  /**
   * Control whether readtime eval is turned on for this port.
   *
   * @see com.ibm.jikes.skij.SchemeTokenizer#readEval
   */
  public void setReadEval(char val) {
    if (tokenizer == null) 
      tokenizer = new SchemeTokenizer(reader);
    tokenizer.readEval = val;
  }

}

