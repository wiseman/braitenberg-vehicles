package com.ibm.jikes.skij;

/* This file is part of Skij.
 * Author: Michael Travers (mt@watson.ibm.com)
 * 
 * Licensed Materials - See the file license.txt.
 * (c) Copyright IBM Corp. 1997, 1998. All rights reserved.
 */

/**
 * Used to restart a saved continuation.
 *
 * @see Continuation
 */
public class ContinuationException extends SchemeException {
  
  Continuation continuation;
  Object result;

  ContinuationException(Continuation c, Object r) {
    super(c + " used outside of its dynamic context");
    continuation = c;
    result = r;
  }

}
  
