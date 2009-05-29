package com.ibm.jikes.skij;
import java.io.*;

/* This file is part of Skij.
 * Author: Michael Travers (mt@watson.ibm.com)
 * 
 * Licensed Materials - See the file license.txt.
 * (c) Copyright IBM Corp. 1997, 1998. All rights reserved.
 */

public class InputPort {

  /* This gets wrapped around an input stream and serves as a Scheme input port.
     Basically all it does is keep an optional listener around in case READ is
     done on the stream. This is necessary because the tokenizer buffers a lot 
     of input so you can't just go on creating new ones. I should write a real
     parser, but this is quicker. 

     Note that mixed reads and read-chars won't work (chars will get lost in the 
     tokenizer). */
  
  public Reader reader;
  SchemeTokenizer tokenizer;

  public InputPort(InputStream s) {
    reader = new InputStreamReader(s); }

  public InputPort(Reader r) {
    reader = r;
  }

  public int readChar() throws java.io.IOException {
    int ch = reader.read(); 
    if (ch == -1)
      throw new java.io.EOFException();
    else
      return ch;
  }

  public int peekChar() throws java.io.IOException {
    reader.mark(1);
    int cha = reader.read();
    reader.reset();
    return cha;
  }

  public Object read() throws java.io.IOException, SchemeException {
    if (tokenizer == null) 
      tokenizer = new SchemeTokenizer(reader);
    return tokenizer.nextThing();
  }

  public void setReadEval(char val) {
    if (tokenizer == null) 
      tokenizer = new SchemeTokenizer(reader);
    tokenizer.readEval = val;
  }

}

