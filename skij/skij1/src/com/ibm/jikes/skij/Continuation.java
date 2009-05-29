package com.ibm.jikes.skij;

/* This file is part of Skij.
 * Author: Michael Travers (mt@watson.ibm.com)
 * 
 * Licensed Materials - See the file license.txt.
 * (c) Copyright IBM Corp. 1997, 1998. All rights reserved.
 */

/**
 * A continuation is a procedure that represents an execution state captured
 * via <code>call-with-current-continuation</code>.
 * When applied, it throws a <code>ContinuationException</code> to perform
 * a transfer of control. Thus a continuation can only be used inside the
 * dynamic context in which it was created (this is a limitation of Skij;
 * full-strength Scheme implementation can re-enter continuations even after
 * their dynamic context has been exited from.
 *
 * @see ContinuationException
 */
public class Continuation extends Procedure {

  String shortClassName() {return "Continuation";}

  public Object apply(Environment within, Cons args) throws SchemeException {
    throw new ContinuationException(this, args.car);
  }
}
  
