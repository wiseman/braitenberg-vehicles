package com.ibm.jikes.skij; 
import java.io.*;

/* This file is part of Skij.
 * Author: Michael Travers (mt@watson.ibm.com)
 * 
 * Licensed Materials - See the file license.txt.
 * (c) Copyright IBM Corp. 1997, 1998. All rights reserved.
 */

/**
 * This implements Scheme output ports by acting as a wrapper around a Java 
 * writer.
 */
public class OutputPort {

  public Writer writer;

  public OutputPort(OutputStream s) {
    writer = new OutputStreamWriter(s); }

  public OutputPort(Writer w) {
    writer = w;
  }

  /**
   * Write a single character.
   */
  public void writeChar(int cha) throws java.io.IOException {
    writer.write(cha); 
    writer.flush();
  }

  /**
   * Write a single character.
   */
  public void writeChar(char cha) throws java.io.IOException {
    writer.write((int)cha); 
    writer.flush();
  }

  /** 
   * Write a string.
   */
  public void write(String string) throws java.io.IOException {
    writer.write(string);
    writer.flush();
  }
}
