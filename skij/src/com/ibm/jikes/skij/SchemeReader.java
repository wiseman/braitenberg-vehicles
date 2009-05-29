package com.ibm.jikes.skij;  
import java.io.*;

/* This file is part of Skij.
 * Author: Michael Travers (mt@watson.ibm.com)
 * 
 * Licensed Materials - See the file license.txt.
 * (c) Copyright IBM Corp. 1997, 1998. All rights reserved.
 */

/**
 * Provides hooks so you can define Java Readers in Skij.
 */
public class SchemeReader extends Reader {

  Procedure read;
  Procedure close;

  /**
   * Create a SchemeReader. 
   * 
   * @param r A Scheme procedure to return characters. The procedure takes 3 arguments: an array to store the characters in, a starting position, and a count. The procedure should retun the number of chars actually read.
   * @param c A Scheme procedure of no arguments to handle EOF conditions.
   * @see java.io.Reader#read
   */
  public SchemeReader(Procedure r, Procedure c) {
    read = r; close = c;
  }

  public int read(char[] chars, int start, int size) throws IOException {
    try {
      return ((Integer)read.apply(Environment.top, Cons.list(chars, new Integer(start), new Integer(size)))).intValue(); }
    catch (SchemeException e) {
      throw new java.io.IOException(e.toString());
    }
  }

  public void close() throws java.io.IOException {
    try {
      close.apply(Environment.top, Nil.nil); }
    catch (SchemeException e) {
      throw new java.io.IOException(e.toString());
    }
  }
}
