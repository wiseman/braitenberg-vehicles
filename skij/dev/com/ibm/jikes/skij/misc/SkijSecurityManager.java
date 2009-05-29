package com.ibm.jikes.skij.misc;
import com.ibm.jikes.skij.*;
import java.io.*;
import java.net.*;

/* This file is part of Skij.
 * Author: Michael Travers (mt@watson.ibm.com)
 * 
 * Licensed Materials - See the file license.txt.
 * (c) Copyright IBM Corp. 1997, 1998. All rights reserved.
 */

/** 
 * Create a security manager whose policy is determined by a Scheme
 * procedure. Note: use with caution.
 *
 */
public class SkijSecurityManager extends SecurityManager {
  
  // +++ option to create with no bypass capability

  boolean bypass = false;
  boolean bypassBypass = false;
  Procedure approvalProc;

  /**
   * The procedure is called with a variable number of arguments. The first
   * is a string specifying the operation that is trying to be approved (same
   * as the method name); the remaining arguments are the arguments to that
   * method. The procedure should return true to approve the operation, or
   * false to cause a SecurityException to be thrown.
   *
   * Note: use with caution. There is no guarantee that a program can't
   * find away around the security checks here. For instance, it might be
   * able to get ahold of the procedure and alter it.
   */
  public SkijSecurityManager(Procedure p) {
    approvalProc = p;
  }

  /**
   * Procedure p must be a thunk, which is run with security checks
   * bypassed.
   */
  public Object bypassing(Procedure p) throws SchemeException {
    return bypassing(p, Environment.top);
  }

  public Object bypassing(Procedure p, Environment e) throws SchemeException {
    if (bypassBypass)
      throw new SchemeException("You can't bypass security now");
	else {
	  Object result;
	  try {
	    bypass = true;
	    synchronized (this) {
	      result = p.apply(e, Nil.nil);
	    }
	  }
	  finally {
	    bypass = false;
	  }
	  return result;
	}
  }

  /** 
   * Run the procedure (a thunk) with the ability to bypass turned off...
   * is this baroque or what?
   */
  public Object noBypassing(Procedure p) throws SchemeException {
    return noBypassing(p, Environment.top);
  }

  public Object noBypassing(Procedure p, Environment e) throws SchemeException {
    Object result;
    try {
      bypassBypass = true;
      synchronized (this) {
	result = p.apply(e, Nil.nil);
      }
    }
    finally {
      bypassBypass = false;
    }
    return result;
  }

  private void check(String operation) {
    check1(Cons.list(operation));
  }

  private void check(String operation, Object arg1) {
    check1(Cons.list(operation, arg1));
  }

  private void check(String operation, Object arg1, Object arg2) {
    check1(Cons.list(operation, arg1, arg2));
  }

  // +++ does this do the right thing for multiple threads? Let's hope so.
  private void check1(Cons args) {
    // on recursive calls, anything goes (the JVM should do this, but no)
    if (!inCheck && !bypass) {		
      inCheck = true;
      try {
	Object result = approvalProc.apply(Environment.top, args); 
	if (result.equals(Boolean.FALSE))
	  throw new SecurityException();
      }
      catch (SchemeException e) {
	System.out.println("Error in security manager:" + e.toString());
      }
      finally {
	inCheck = false;
      }
    }
  }

  // Member access is so fundamental to Skij, we always allow it
  public void checkMemberAccess(Class clazz, int which) {
    //    check("checkMemberAccess", clazz, new Integer(which));
  }

  public void checkCreateClassLoader() {
    check("checkCreateClassLoader");
  }

  public void checkAccess(Thread g) {
    check("checkAccess", g);
  }

  public void checkAccess(ThreadGroup g) {
    check("checkAccess", g);
  }

  public void checkExit(int status) {
    check("checkExit", new Integer(status));
  }

  public void checkExec(String cmd) {
    check("checkExec", cmd);
  }

  public void checkLink(String lib) {
    check("checkLink", lib);
  }

  public void checkRead(FileDescriptor fd) {
    check("checkRead", fd);
  }

  public void checkRead(String file) {
    check("checkRead", file);
  }

  public void checkRead(String file, Object context) {
    check("checkRead", file, context);
  }

  public void checkWrite(FileDescriptor fd) {
    check("checkWrite", fd);
  }

  public void checkWrite(String file) {
    check("checkWrite", file);
  }

  public void checkDelete(String file) {
    check("checkDelete", file);
  }

  public void checkConnect(String host, int port) {
    check("checkConnect", host, new Integer(port));
  }

  public void checkConnect(String host, int port, Object context) {
    check("checkConnect", new Integer(port), context);
  }

  public void checkListen(int port) {
    check("checkListen", new Integer(port));
  }

  public void checkAccept(String host, int port) {
    check("checkAccept", host, new Integer(port));
  }

  public void checkMulticast(InetAddress maddr) {
    check("checkMulticast", maddr);
  }

  public void checkMulticast(InetAddress maddr, byte ttl) {
    check("checkMulticast", maddr, new Integer(ttl));
  }

  public void checkPropertiesAccess() {
    check("checkPropertiesAccess");
  }

  public void checkPropertyAccess(String key) {
    check("checkPropertyAccess", key);
  }

// unlike all others, this one returns a value...argh
//   public boolean checkTopLevelWindow(Object window) {
//     check("checkTopLevelWindow", window);
//   }

  public void checkPrintJobAccess() {
    check("checkPrintJobAccess");
  }

  public void checkSystemClipboardAccess() {
    check("checkSystemClipboardAccess");
  }

  public void checkAwtEventQueueAccess() {
    check("checkAwtEventQueueAccess");
  }

  public void checkPackageAccess(String pkg) {
    check("checkPackageAccess", pkg);
  }

  public void checkPackageDefinition(String pkg) {
    check("checkPackageDefinition", pkg);
  }

  public void checkSetFactory() {
    check("checkSetFactory");
  }

  public Class[] getClassStack() {
    return getClassContext();
  }

}
