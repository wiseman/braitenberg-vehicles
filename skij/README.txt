This package contains Skij, a Scheme interpreter written in Java that
allows you to create and manipulate Java objects interactively. Skij 
should run under any JDK 1.1 compliant Java.

The contents of the distribution file skijdist.zip can be unpacked with
unzip or jar. Its contents are:

  README.txt	-- this file
  skij.jar	-- the skij classes and libraries
  skij.bat	-- a Windows shell script to start Skij
  skij.sh	-- a Unix shell script to start Skij
  doc/skij.html -- documentation
  javadoc	-- documentation of internals in javadoc format
  examples/ 	-- examples of Skij programs

See the HTML documentation for more information.

Michael Travers (mt@watson.ibm.com)
Java Tools Group
IBM T. J. Watson Research Center

Release Notes:

Version 1.7.2
- minor tweaks and name-changes

Version 1.7.1
Correct a few bugs:
- In JDK 1.2, some methods did not get set accessible
- Setting the value of a rest binding would sometimes lose data
- trace no longer gets confused about level
- separate out source files since compiler can't handle mix.

Version 1.7 (February 99)
New features:
- Object graph inspector: <tt>(graph-inspect <i>&lt;obj></i>)</tt>
- Javadoc for Skij internals
Bug fixes:
- fix for cond
- fix for Swing listeners
- allow alternate defmacro syntax: (defmacro name (arg...) . body)
- fix bug in method invocation; was missing some methods when primtive types were involved.
- fix constructor lookup bug
- speedups by moving Nil lookup out of inner loops
- use classes rather than version string to determine what flavor of Java we have.
- removed some unused .misc classes.

Version 1.6 (December 98)

- Fixes for JDK 1.2
- better support for classloaders
- -window init option: start up in window instead of console
- added fluid-let
- instanceof and other procs made into primitives
- significant speedups
- bug fix for bound?
- cond supports else and => properly
- added method-apropos

Version 1.5 (November 98)
New features:
- run-applet
- where-is to find library functions
- java-backtrace available for appropriate exceptions
- -quiet init option suppresses some messages

Bug fixes:
- fix interaction between apropos and autoloading
- better handling of encapsulated exceptions
- load wasn't closing files properly
- dynamic-winds to handle file closing in other cases
- start-application was buggy
- reorganized some libraries
- fixed nasty bug in apply

Version 1.4 (October 98)
New features:
- libraries can load from .class files
- apropos
- flags to control tail-calling and saving macro sources

Bug fixes:
- better direction of error messages and other console output
- applet support
- get-method and other reflection tools
- string output quotes properly

Version 1.2 (August 98)

New features:
- setf, incf, decf, push (modelled on Common Lisp features)
- can compile under both JDK 1.1 and 1.2
- much improved autoload
- file loading is much faster

Bug fixes:
- errors in loaded files are handled better
- catch/continuation interactions are fixed
- some fixes for instanceof
- toString for lists containing null elements

Version 1.1 (Bastille Day 98)

New Features:
- support for access to nonpublic members in JDK 1.2
- backtrace
- more flexibility in define-memoized
- more support for JDK1.2
- faster top-level environments; can store null

Bug fixes:
- dynamic var lookup
- dynamic lookup and recursion was causing stack growth under tailcalling
- set! of dotted arg var
- inexact->exact
- start-application

Version 1.0 (May 98):
- Welcome to Skij
