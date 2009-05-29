package com.ibm.jikes.skij.util;
import java.lang.reflect.*;
import java.util.Vector;
import java.util.Hashtable;

/* This file is part of Skij.
 * Author: Michael Travers (mt@watson.ibm.com)
 * 
 * Licensed Materials - See the file license.txt.
 * (c) Copyright IBM Corp. 1997, 1998. All rights reserved.
 */

/**
 * The <code>Dynvoke</code> class implements a high-level interface
 * to Java reflection. Using Dynvoke, you can dynamically create
 * objects of arbitrary classes, send methods to them,  examine
 * and change their fields.
 * <p>
 * In versions of Java prior to Java 2, only public classes and members
 * can be accessed reflectively. In Java 2, nonpublic classes and members
 * can be accessed if the security manager allows it. <code>Dynvoke</code> will try
 * the appropriate method in either case.
 * <p>
 * Primitive values may be required by some methods or constructors,
 * may be returned as values by some methods, and may be used as the
 * value for some fields. In such cases, the methods of <code>Dynvoke</code>
 * accept or return the wrapped form (i.e., a <code>java.lang.Integer</code> object will
 * be used for values of type <code>int</code>).
 *
 * @author  Michael Travers
 * @version 1.7 1/14/99
 * @see     com.ibm.jikes.skij.util.ExtendableClassLoader
 * @see     java.lang.reflect.Method
 * @see     java.lang.reflect.Constructor
 * @see     java.lang.reflect.Field
 */

public class Dynvoke {

  /**
   * True if we are running in JDK1.1 or above. Reflection can be used iff this variable is <code>true</true>.
   */
  public static final boolean java11 = 
  isClassPresent("java.lang.reflect.Method");

  /**
   * True if we are running in Java 2 or above. If this is <code>true</true>, then non-public classes and members can be accessed.
   */
  public static final boolean java2 = 
  isClassPresent("java.lang.reflect.AccessibleObject");

  //// Method invocation

  /**
   * Invoke a method on an object, given the object, a method name, and 
   * an array of arguments.  The appropriate method is found as defined
   * in the Java Language Specification, section 15.11, using the dynamic
   * types of the arguments as substitutes for the static type information
   * used in normal method invocation. If an appropriate method is found,
   * it is invoked on the object and arguments using standard reflection.
   *
   * @param obj the target object.
   * @param methodName the name of the method to invoke.
   * @param args the arguments to the method, in the form of an array.
   * @return the result of the method invocation (possibly null).
   * @exception NoSuchMethodException if no suitable method can be found.
   * @exception Throwable the method might throw an arbitrary exception.
   */
  public static Object invoke(Object obj, String methodName, Object[] args) throws NoSuchMethodException, Throwable {
    return invoke(obj.getClass(), obj, methodName, args); 
  }

  /**
   * Invoke a method on an object or class, given the class, object,
   * a method name, and an array of arguments. This form of <code>invoke</code>
   * is useful for invoking static methods.
   *
   * @param myClass the target class.
   * @param obj the target object (ignored and can be null for static methods).
   * @param methodName the name of the method to invoke.
   * @param args the arguments to the method, in the form of an array.
   * @return the result of the method invocation (possibly null).
   * @exception NoSuchMethodException if no suitable method can be found.
   * @exception Throwable the method might throw an arbitrary exception.
   */
  public static Object invoke(Class myClass, Object obj, String methodName, Object[] args) throws NoSuchMethodException, Throwable {
    Class[] signature = classArray(args);    
    Method method = lookupMethod(myClass, methodName, signature);
    if (method == null)
      throw new NoSuchMethodException("no applicable method");
    else {
      try {
	return method.invoke(obj, args); }
      // pass exceptions upward.
      catch (InvocationTargetException e) {
	throw(e.getTargetException()); }
    }
  }

  /**
   * Return a method object, given a target object, method name, and arguments.
   * This is analogous to <code>invoke</code>, but returns the appropriate method object
   * rather than actually performing the invocation.
   *
   * @param obj the target object (ignored and can be null for static methods).
   * @param methodName the name of the method to invoke.
   * @param args the arguments to the method, in the form of an array.
   * @return a method object.
   * @exception NoSuchMethodException if no suitable method can be found.
   */
  public static Object getMethod(Object obj, String methodName, Object[] args) throws NoSuchMethodException {
    return getMethod(obj.getClass(), obj, methodName, args); 
  }
  

  /**
   * Return a method object, given a target object or class, method name, and arguments.
   *
   * @param myClass the target class.
   * @param obj the target object (ignored and can be null for static methods).
   * @param methodName the name of the method to invoke.
   * @param args the arguments to the method, in the form of an array.
   * @return a method object
   * @exception NoSuchMethodException if no suitable method can be found.
   */
   public static Method getMethod(Class myClass, Object obj, String methodName, Object[] args) throws NoSuchMethodException {
    Class[] signature = classArray(args);    
    return lookupMethod(myClass, methodName, signature);
  }

  /* Here is our arsenal of techniques:
     1) direct lookup (getMethod) (fast, but only works sometimes, and will cause an exception when it doesn't work)
     2) indirect lookup (slow)
     3) cached lookup (not that fast, because it has to create a key object)

     The strategy now is to try direct first, then cached, than indirect.  

     The alternative is (cache, direct, indirect), in other words, cache directs.  That may or may not be better.

     Also: right now indirect works through getMethods. Using getDirectMethods might be better.

     Note: if indirects are not cached, they should be less careless about calling trace functions.
   */


  // top-level -- try easy case first
  static Method lookupMethod(Class targetClass, String name, Class[] argClasses) throws NoSuchMethodException {  
    // first, see if there is an exact match

    // ELIMINATED because it turns out a new method object is generated each time!  Yuck!
    // I wonder if this is true for all VMs though.

//     try {
//       return targetClass.getMethod(name, argClasses); }
//     catch (NoSuchMethodException e) {	
//       if (!java2 && argClasses.length == 0) {  // if no args and no exact match, we're out of luck
// 	return null; }
//     }
    // go the more complicated route
    return lookupMethodCached(targetClass, name, argClasses); 
  }

  // +++ when JDK 1.2 comes out, use weak references
  static Hashtable hash = new Hashtable();

  // synchronized since the key is reused
  static synchronized Method lookupMethodCached(Class target, String name, Class[] argClasses) throws NoSuchMethodException  {

    MethodKey key = MethodKey.make(target, name, argClasses);
    Object result = hash.get(key);
    if (result == null) {
      Method nresult = lookupMethodLaboriously(target, name, argClasses);
      if (nresult == null)
	return null;
      else {
	key.keep();
	hash.put(key, nresult);
	return nresult; }}
    else
      return (Method)result; }

  static Method lookupMethodLaboriously(Class target, String name, Class[] argClasses) throws NoSuchMethodException {

    // first try for exact match
    try {
      Method m = target.getMethod(name, argClasses); 
      if (java2) setAccessible(m, true);
      return m;
    }
    catch (NoSuchMethodException e) {	
      if (!java2 && argClasses.length == 0) {  // if no args and no exact match, out of luck
 	return null; }
    }

     // go the more complicated route
    if (java2) 
      return lookupMethodLaboriously(new Vector(), target, name, argClasses);
    else {
      Method[] methods = target.getMethods();
      Vector goodMethods = new Vector();
      for (int i = 0; i != methods.length; i++) {
	if (name.equals(methods[i].getName()) &&
	    matchClasses(methods[i].getParameterTypes(), argClasses))
	  goodMethods.addElement(methods[i]);
      }
      switch (goodMethods.size()) {
      case 0: {
	return null; }
      case 1: {
	return (Method)goodMethods.firstElement(); }
      default: {
	return mostSpecificMethod(goodMethods);
      }}
    }
  }

  // jdk1.2 version 
  static Method lookupMethodLaboriously(Vector goodMethods, Class target, String name, Class[] argClasses) throws NoSuchMethodException {

    if (target == null) {
      Method m;
      switch (goodMethods.size()) {
      case 0: {
	return null; }
      case 1: {
	m = (Method)goodMethods.firstElement(); }
      default: {
	m = mostSpecificMethod(goodMethods);
      }}
      setAccessible(m, true);
      return m;
    }
    else {
      Method[] methods = target.getDeclaredMethods();
      for (int i = 0; i != methods.length; i++) {
	Class[] paramTypes = methods[i].getParameterTypes();
	if (name.equals(methods[i].getName()) &&
	    matchClasses(paramTypes, argClasses))
	  goodMethods.addElement(methods[i]);
      }
      return lookupMethodLaboriously(goodMethods, target.getSuperclass(), name, argClasses);
    }
  }

  /**
   * Create and return an object of a given class, invoking the constructor appropriate
   * for the arguments.
   *
   * @param myClass the class of the object to be created.
   * @param args the arguments to the constructor, in the form of an array.
   * @return the new object.
   * @exception NoSuchMethodException if no suitable constructor can be found.
   * @exception Throwable the constructor might throw an arbitrary exception.
   */
  public static Object create(Class myClass, Object[] args) throws Throwable {
    Class[] signature = classArray(args);    
    Constructor[] constructors = java2 ? myClass.getDeclaredConstructors() : myClass.getConstructors();
    Vector goodConstructors = new Vector();
    for (int i = 0; i != constructors.length; i++) {
      if (matchClasses(constructors[i].getParameterTypes(), signature))
	goodConstructors.addElement(constructors[i]);
    }
    Constructor c;
    switch (goodConstructors.size()) {
    case 0: {
      throw new NoSuchMethodException("no applicable constructor"); }
    case 1: {
      c = (Constructor)goodConstructors.firstElement(); }
    default: {
      c = mostSpecific(goodConstructors);
    }
    }
    if (java2) setAccessible(c, true);
    try {
      return c.newInstance(args);
    }
    catch (InvocationTargetException e) {
      throw(e.getTargetException()); 
    }
  }

  //// Peek and Poke

  /**
   * Set the value of a field.
   * 
   * @param object the target object.
   * @param fieldName the name of the field as a string.
   * @param value the new value for the field.
   * @exception NoSuchFieldException if the field can't be found
   * @exception IllegalAccessException if the field can't be accessed.
   * @exception IllegalArgumentException if the new value is not suitable for the field.
   */
  public static void poke(Object object, String fieldName, Object value)
       throws IllegalAccessException, NoSuchFieldException {
	 Class myClass = object.getClass();
	 poke(object.getClass(), object, fieldName, value); 
  }

  /**
   * Set the value of a field. This form is useful for static fields, in
   * which case <code>object</code> is ignored and may be null.
   * 
   * @param myClass the target class 
   * @param object the target object (ignored and may be null for static fields).
   * @param fieldName the name of the field as a string.
   * @exception NoSuchFieldException if the field can't be found
   * @exception IllegalAccessException if the field can't be accessed.
   * @exception IllegalArgumentException if the new value is not suitable for the field.
   */
  public static void poke(Class myClass, Object object, String fieldName, Object value)
       throws IllegalAccessException, NoSuchFieldException {
	 Field field = fieldLookup(myClass, fieldName);
	 field.set(object, value); 
  }

  /**
   * Return the value of a field.
   * 
   * @param object the target object.
   * @param fieldName the name of the field as a string.
   * @exception NoSuchFieldException if the field can't be found
   * @exception IllegalAccessException if the field can't be accessed.
   */
  public static Object peek(Object object, String fieldName) 
       throws IllegalAccessException, NoSuchFieldException {
	 return peek(object.getClass(), object, fieldName);
  }

  /**
   * Return the value of a field. This form is useful for static fields, in
   * which case <code>object</code> is ignored and may be null.
   * 
   * @param myClass the target class 
   * @param object the target object (ignored and may be null for static fields).
   * @param fieldName the name of the field as a string.
   * @exception NoSuchFieldException if the field can't be found
   * @exception IllegalAccessException if the field can't be accessed.
   */
  public static Object peek(Class myClass, Object object, String fieldName) 
       throws IllegalAccessException, NoSuchFieldException {
	 Field field = fieldLookup(myClass, fieldName);
	 return field.get(object); 
  }

  static Field fieldLookup(Class myClass, String fieldName) throws NoSuchFieldException {
    Field field;
    try {
      field = myClass.getField(fieldName);	   
    }
    catch (NoSuchFieldException e) {
      if (java2)
	field = fieldLookup0(myClass, fieldName);	// try harder
      else
	throw e;
    }
    if (java2) setAccessible(field, true);	// public fields can be inaccessible
    return field;
  }

  // if it's a non-public field, use this uglier method
  static Field fieldLookup0(Class myClass, String fieldName) throws NoSuchFieldException {
    Field[] localFields = myClass.getDeclaredFields();
    for (int i = 0; i != localFields.length; i++) {
      if (fieldName.equals(localFields[i].getName())) {
	Field f = localFields[i];
	return f;
      }
    }
    // didn't find it, go up to superclass
    Class sup = myClass.getSuperclass();
    if (sup == null)
      throw new NoSuchFieldException(fieldName);
    else
      return fieldLookup(sup, fieldName);
  }


  // go thru reflection so I can compile this damned thing under 1.1
  static Method setAccessibleMethod = null;

  static void setAccessible(Object thing, boolean accessible) {
    try {
      if (setAccessibleMethod == null) {
	Class aclass = Class.forName("java.lang.reflect.AccessibleObject");
	setAccessibleMethod =
	  aclass.getMethod("setAccessible", new Class[]{ booleanClass });
      }
      setAccessibleMethod.invoke(thing, new Object[]{ Boolean.TRUE }); // +++ array can be cached
    }
    catch (Throwable e) {
      System.out.println("Error trying to set accessibility for " + thing);
    }
  }

  //// Utilities

  // 1st arg is from method, 2nd is actual parameters
  static boolean matchClasses(Class[] mclasses, Class[] pclasses) {
    if (mclasses.length == pclasses.length) {
      for (int i = 0; i != mclasses.length; i++) {
	if (!matchClass(mclasses[i], pclasses[i])) {
	  return false; }
      }
      return true;
    }
    return false;
  }

  static Class NullClass;
  static Class nullClass;
  static Class BooleanClass;
  static Class booleanClass;
  static Class CharacterClass;
  static Class characterClass;
  static Class ByteClass;
  static Class byteClass;
  static Class ShortClass;
  static Class shortClass;
  static Class IntegerClass;
  static Class integerClass;
  static Class LongClass;
  static Class longClass;
  static Class FloatClass;
  static Class floatClass;
  static Class DoubleClass;
  static Class doubleClass;

  // have to use static initializer because of exceptions
  static {
    try {
      nullClass = Void.TYPE;
      NullClass = Class.forName("java.lang.Void");
      booleanClass = Boolean.TYPE;
      BooleanClass = Class.forName("java.lang.Boolean");
      characterClass = Character.TYPE;
      CharacterClass = Class.forName("java.lang.Character");
      byteClass = Byte.TYPE;
      ByteClass = Class.forName("java.lang.Byte");
      shortClass = Short.TYPE;
      ShortClass = Class.forName("java.lang.Short");
      integerClass = Integer.TYPE;
      IntegerClass = Class.forName("java.lang.Integer");
      longClass = Long.TYPE;
      LongClass = Class.forName("java.lang.Long");
      floatClass = Float.TYPE;
      FloatClass = Class.forName("java.lang.Float");
      doubleClass = Double.TYPE;
      DoubleClass = Class.forName("java.lang.Double");
    }
    catch (ClassNotFoundException e) {
      System.out.println("this shouldn't happen");
    }
  }
      
  // 1st arg is from method, 2nd is from actual parameter
  // also called by moreSpecific (so pclass could be primitive)
  // note that this reduces to isAssignableFrom for reference classes
  static boolean matchClass(Class mclass, Class pclass) {
    if (pclass.isPrimitive())
      pclass = wrappedClass(pclass);
    if (mclass.isPrimitive()) {
      return 
	wrappedClass(mclass) == pclass ||
	isPrimAssignableFrom(mclass, pclass);
    }
    else			// mclass is a reference type
      return 
	mclass == pclass ||
	mclass.isAssignableFrom(pclass) || 
	pclass == NullClass;
  }


  // mclass is a primitive Class, pclass is a wrapper Class
  // this assumes that mclass and pclass are not "identical" (modulo wrapping)
  // see JLS 5.1.2 (Widening Primitve Conversions)
  static boolean isPrimAssignableFrom(Class mclass, Class pclass) {
    if (pclass == ByteClass)
      return
	mclass == shortClass ||
	mclass == integerClass ||
	mclass == longClass ||
	mclass == floatClass ||
	mclass == doubleClass;
    else if (pclass == IntegerClass || pclass == ShortClass || pclass == CharacterClass)
      return
	mclass == integerClass ||
	mclass == longClass ||
	mclass == floatClass ||
	mclass == doubleClass;
    else if (pclass == LongClass || pclass == FloatClass)
      return
	mclass == floatClass ||
	mclass == doubleClass;
    return false;
  }
	

  static Class wrappedClass(Class clazz) {
    if (clazz.isPrimitive()) {
      Class wrappedClass;
      if (clazz == booleanClass)
	wrappedClass = BooleanClass;
      else if (clazz == integerClass)
	wrappedClass = IntegerClass;
      else if (clazz == characterClass)
	wrappedClass = CharacterClass;
      else if (clazz == byteClass)
	wrappedClass = ByteClass;
      else if (clazz == shortClass)
	wrappedClass = ShortClass;
      else if (clazz == longClass)
	wrappedClass = LongClass;
      else if (clazz == floatClass)
	wrappedClass = FloatClass;
      else if (clazz == doubleClass)
	wrappedClass = DoubleClass;
      else throw new Error("Unknown primitive type" + clazz);
      return wrappedClass;
    }
    else 
      return clazz;
  }
  
  static Constructor mostSpecific(Vector constructors) throws Throwable {
    for (int i = 0; i != constructors.size(); i++) {
      for (int j = 0; j != constructors.size(); j++) {
	if ((i != j) &&
	    (moreSpecific((Constructor)constructors.elementAt(i), (Constructor)constructors.elementAt(j)))) {
	  constructors.removeElementAt(j);
	  if (i > j) i--;
	  j--;
	}
      }
    }
    if (constructors.size() == 1)
      return (Constructor)constructors.elementAt(0);
    else
      throw new NoSuchMethodException(">1 most specific constructor");
  }

  // true if c1 is more specific than c2
  static boolean moreSpecific(Constructor c1, Constructor c2) {
    Class[] p1 = c1.getParameterTypes();
    Class[] p2 = c2.getParameterTypes();
    int n = p1.length;
    for (int i = 0; i != n; i++) {
      if (matchClass(p1[i], p2[i])) {
	return false;
      }
    }
    return true;
  }

  // these are exactly the same as above except for types, which suggests (duh) that
  // Method and Constructor should have a common ancestor or interface.
  static Method mostSpecificMethod(Vector methods) throws NoSuchMethodException {
    for (int i = 0; i != methods.size(); i++) {
      for (int j = 0; j != methods.size(); j++) {
	if ((i != j) &&
	    (moreSpecific((Method)methods.elementAt(i), (Method)methods.elementAt(j)))) {
	  methods.removeElementAt(j);
	  if (i > j) i--;
	  j--;
	}
      }
    }
    if (methods.size() == 1)
      return (Method)methods.elementAt(0);
    else
      throw new NoSuchMethodException(">1 most specific method");
  }

  // true if c1 is more specific than c2
  static boolean moreSpecific(Method c1, Method c2) {
    if (!matchClass(c2.getDeclaringClass(), c1.getDeclaringClass())) // needed for jdk12 only
      return false;
    Class[] p1 = c1.getParameterTypes();
    Class[] p2 = c2.getParameterTypes();
    int n = p1.length;
    for (int i = 0; i != n; i++) {
      if (!matchClass(p2[i], p1[i])) {
	return false;
      }
    }
    return true;
  }

  // given an array of objects, return an array of corresponding classes
  static Class[] classArray(Object[] args) {
    Class[] classes = new Class[args.length];
    for (int i = 0; i != args.length; i = i + 1)
      classes[i] = ((args[i] == null) ? NullClass : args[i].getClass());
    return classes; 
  }

  static boolean isClassPresent(String className) {
    try {
      Class.forName(className);
    }
    catch (ClassNotFoundException e) {
      return false;
    }
    return true;
  }
} // end class Dynvoke

//// these objects are used as keys to search the method cache.

class MethodKey {
    Class target;
    String name;
    Class[] argClasses;

  static MethodKey spareKey;

  // use this instead of a constructor (but beware of concurrency)
  static MethodKey make(Class t,String n, Class[] a) {
    if (spareKey == null)
      spareKey = new MethodKey(t, n, a);
    else
      spareKey.setup(t, n, a);
    return spareKey;
  }

  void setup(Class t,String n, Class[] a) {
    target = t; name = n; argClasses = a; }

  void keep() {
    spareKey = null;
  }

  public String toString() {
    return "<<" + target + "." + name + toStringArray(argClasses) + ">>"; }

  // Built-in array printing is lousy, so use this. Could be more generally useful
  static String toStringArray(Object[] array) {
    java.io.StringWriter out = new java.io.StringWriter();
    out.write('[');
    for (int i = 0; i != array.length; i++) {
      out.write(array[i].toString());
      out.write(' '); }
    out.write(']');
    return out.toString(); 
  }

  MethodKey(Class t,String n, Class[] a) {
    target = t; name = n; argClasses = a; }

  public boolean equals(Object mx) {
    if (mx instanceof MethodKey) {
      MethodKey m = (MethodKey)mx;
      boolean v =
	(target == m.target &&
	 name.equals(m.name) &&
	 arrayEquals(argClasses, m.argClasses));
      return v; }
    else
      return false;
  }

  static boolean arrayEquals(Class[] c1, Class[] c2) {
    if (c1.length == c2.length) {
      for (int i = 0; i != c1.length; i++) {
	if (c1[i] != c2[i])
	  return false;}
      return true; }
    else
      return false;
  }

  public int hashCode() {
    int v = target.hashCode() ^ (37*name.hashCode());
    for (int i = 0; i != argClasses.length; i++) // added this, but seems to make things slower...
      v = (v * 43) ^ argClasses[i].hashCode();
    return v;
  }
}
