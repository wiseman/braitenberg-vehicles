package com.ibm.jikes.skij.util;

import java.util.*;
import java.io.*;
import java.util.zip.*;
import java.util.jar.*;

/* This file is part of Skij.
 * Author: Michael Travers (mt@watson.ibm.com)
 * 
 * Licensed Materials - See the file license.txt.
 * (c) Copyright IBM Corp. 1997, 1998. All rights reserved.
 */


/**
 * This class allows you to load classes from files that are not on the standard
 * Java CLASSPATH. You can make a loader that loads from some combination of .class,
 * .zip, or .jar files.
 * 
 * Note: will try to use JDK 1.2 classes for JAR files. These classes are accessed
 * reflectively to avoid verification problems in earlier JDKs.
 *
 * @author Michael Travers
 * @version 1.7 1/14/99
 * Based on earlier versions by Chuck McManis and O. Dedieu.
 */
public class ExtendableClassLoader extends ClassLoader {

  private Hashtable cache;
  public Vector paths;		
  private boolean jarFileSupport = isClassPresent("java.util.jar.JarFile");

  /**
   * Constructs a new class loader and initializes it.
   */
  public ExtendableClassLoader() {
    cache = new Hashtable();
    paths = new Vector();
  }

  /**
   * Add an element to the class path.  The element can be the name of a .class, .zip,
   * or .jar file.
   *
   * @param path the class path
   */
  public void addClassPath(String path) {
    paths.addElement(path);
  }

  /**
   * Remove a class path element. Classes already loaded from this path are not
   * affected.
   *
   * @param path the class path to remove
   */
  public void removeClassPath(String path) {
    for(Enumeration e = paths.elements(); e.hasMoreElements();) {
      String p = (String)e.nextElement();
      if (p.equals(path)) {
        paths.removeElement(p);
        break;
      }
    }
  }

  // it is assumed that the file separator is a one-character string
  final static String fsep = System.getProperty("file.separator");

  private String classNameToFileName(String className) {
    return className.replace('.', fsep.charAt(0)) + ".class";
  }

  private byte[] getClassFromAddedClassPaths(String className) throws ClassNotFoundException {

    String fileName = classNameToFileName(className);
    try {
      InputStream s = getResourceAsStream(fileName);
      if (s == null) throw new ClassNotFoundException(className);
      return streamToBytes(s);
    }
    catch (IOException e) {
      throw new ClassNotFoundException(className);
    }
  }

  private Hashtable zipHash = new Hashtable();

  private static String fileType(String path) {
    int dotpos = path.lastIndexOf(".");
    if (dotpos > 0)
      return path.substring(1 + dotpos, path.length());
    else
      return null;
  }

  private ZipFile pathZip(String path) throws IOException {
    Object memoized = zipHash.get(path);
    if (memoized == null) {
      ZipFile zip = null;
      if (jarFileSupport && fileType(path).equals("jar"))
	zip = new JarFile(path);
      else
	zip = new ZipFile(path);
      zipHash.put(path, zip);
      return zip;
    }
    else
      return (ZipFile)memoized;
  }
      

  // overrides ClassLoader
  public InputStream getResourceAsStream(String resName) {

    String zipName = resName.replace(fsep.charAt(0),'/');

    for(Enumeration enum = paths.elements(); enum.hasMoreElements();) {
      String path = (String)enum.nextElement();
      try {
	if (zipFileP(path)) {
	  ZipFile zip = pathZip(path);
	  ZipEntry entry = zip.getEntry(zipName);

 	  if (entry != null) {
	    return zip.getInputStream(entry);
	  }
	}
	else {
	  File f = new File(path + fsep + resName);
	  if (f.exists()) {
	    return new FileInputStream(f);
	  }
	}
      }
      catch (Throwable e) {
	System.out.println("Error reading resource " + resName + " from " + path + ": " + e.toString());
      }
    }
    return null;
  }

  static byte[] streamToBytes(InputStream in) throws IOException {
    InputStream xin = new DataInputStream (new BufferedInputStream(in));
    byte[] result = new byte[in.available()];
    xin.read(result);
    return result;
  }

  static boolean zipFileP(String path) {
    return
      path.endsWith(".zip") ||
      path.endsWith(".jar");
  }

  /**
   * Requests the class loader to load a class with the specified
   * name. The loadClass method is called by the Java Virtual Machine
   * when a class loaded by a class loader first references another
   * class.
   *
   * @param name the name of the desired class.
   * @return the resulting <code>Class</code>, or <code>null</code> if
   * it was not found.
   * @exception ClassNotFoundException
   * if the class loader cannot find a definition for the class. 
   */
  public Class loadClass(String className) throws ClassNotFoundException {
    return (loadClass(className, true));
  }

  /**
   * Resolves the specified name to a Class. The method loadClass() is
   * called by the virtual machine.
   *
   * @param name the name of the desired class.
   * @param resolve true if the class needs to be resolved. 
   * @return the resulting <code>Class</code>, or <code>null</code> if
   * it was not found.
   * @exception ClassNotFoundException
   * if the class loader cannot find a definition for the class. 
   */
  public synchronized Class loadClass(String className, boolean resolveIt) throws ClassNotFoundException {
    Class result;
    byte classData[];

    // Check the cache of classes
    result = (Class)cache.get(className);
    if (result != null) {
      return result;
    }

    // Check with the primordial class loader 
    try {
      result = super.findSystemClass(className);
      return result;
    }
    catch (ClassNotFoundException e) {
    }

    // Try to load it from the added class paths
    classData = getClassFromAddedClassPaths(className);

    // Define it (parse the class file)
    result = defineClass(className, classData, 0, classData.length);
    if (result == null) {
      throw new ClassFormatError();
    }

    if (resolveIt) {
      resolveClass(result);
    }

    // Add the class to the cache
    cache.put(className, result);
    
    return result;
  }

  // utility
  static boolean isClassPresent(String className) {
    try {
      Class.forName(className);
    }
    catch (ClassNotFoundException e) {
      return false;
    }
    return true;
  }

}


