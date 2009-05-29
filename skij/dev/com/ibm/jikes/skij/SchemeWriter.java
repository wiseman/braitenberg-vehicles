package com.ibm.jikes.skij;
import java.io.*;


/* This file is part of Skij.
 * Author: Michael Travers (mt@watson.ibm.com)
 * 
 * Licensed Materials - See the file license.txt.
 * (c) Copyright IBM Corp. 1997, 1998. All rights reserved.
 */

/**
 * Provides hooks so you can define Java Writers in Skij.
 */
public class SchemeWriter extends Writer {
  
  Procedure write;
  Procedure close;
  Procedure flush;

  /**
   * Create a SchemeWriter. 
   * 
   * @param r A Scheme procedure that accepts characters to be written. The procedure should take 3 arguments: an array of chars, a start position, and a count.
   * @param c A Scheme procedure of no arguments to handle EOF conditions.
   * @param f A Scheme procedure of no arguments that flushes the buffers.
   * @see java.io.Writer#write
   * @see java.io.Writer#close
   * @see java.io.Writer#flush
   */
  public SchemeWriter(Procedure w, Procedure c, Procedure f) {
    write = w; close = c; flush = f;
  }

  public void write(char[] chars, int start, int size) throws IOException {
    try {
      write.apply(Environment.top, Cons.list(chars, new Integer(start), new Integer(size))); }
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

  public void flush() throws java.io.IOException {
    try {
      flush.apply(Environment.top, Nil.nil); }
    catch (SchemeException e) {
      throw new java.io.IOException(e.toString());
    }
  }
}

