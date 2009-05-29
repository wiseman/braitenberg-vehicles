package com.ibm.jikes.skij.misc;
import com.ibm.jikes.skij.*;
import java.awt.event.*;

/* This file is part of Skij.
 * Author: Michael Travers (mt@watson.ibm.com)
 * 
 * Licensed Materials - See the file license.txt.
 * (c) Copyright IBM Corp. 1997, 1998. All rights reserved.
 */

/**
 * This class lets you handle AWT events with Scheme procedures.
 * It implements all of the AWT Listener interfaces, and so can be 
 * used as any type of Listener. The class contains a Scheme procedure
 * of one argument. This procedure is called, with the event as an
 * argument, whenever one of the Listener methods is invoked by
 * the window system.
 * <p>An example of use:
 * <pre>
 *   (define b (new 'java.awt.Button "Press Me"))
 *   (define listener (new 'com.ibm.jikes.skij.misc.GenericCallback 
 *                       (lambda (evt)
 *                         (print "Thanks, that felt good"))))
 *   (invoke b 'addActionListener listener)
 *</pre>
 */
public class GenericCallback implements ActionListener, AdjustmentListener, ComponentListener, ContainerListener, WindowListener, MouseListener, KeyListener, FocusListener, ItemListener, MouseMotionListener, TextListener {

  Procedure proc;

  /** 
   * Create a GenericCallback. <code>p</code> is the procedure to handle events.
   */
  public GenericCallback(Procedure p) {
    proc = p;
  }
  
  public void actionPerformed(ActionEvent evt) {
    handleEvent(evt); }

  public void adjustmentValueChanged(AdjustmentEvent evt) {
    handleEvent(evt); }

  public void componentHidden(ComponentEvent evt) {
    handleEvent(evt); }
  public void componentMoved(ComponentEvent evt) {
    handleEvent(evt); }
  public void componentResized(ComponentEvent evt) {
    handleEvent(evt); }
  public void componentShown(ComponentEvent evt) {
    handleEvent(evt); }

  public void componentAdded(ContainerEvent evt) {
    handleEvent(evt); }
  public void componentRemoved(ContainerEvent evt) {
    handleEvent(evt); }

  public void textValueChanged(TextEvent evt) {
    handleEvent(evt); }

  public void windowActivated(WindowEvent evt) {
    handleEvent(evt); }
  public void windowClosed(WindowEvent evt) {
    handleEvent(evt); }
  public void windowClosing(WindowEvent evt) {
    handleEvent(evt); }
  public void windowDeactivated(WindowEvent evt) {
    handleEvent(evt); }
  public void windowDeiconified(WindowEvent evt) {
    handleEvent(evt); }
  public void windowIconified(WindowEvent evt) {
    handleEvent(evt); }
  public void windowOpened(WindowEvent evt) {
    handleEvent(evt); }

  public void mouseClicked(MouseEvent evt) {
    handleEvent(evt); }
  public void mouseEntered(MouseEvent evt) {
    handleEvent(evt); }
  public void mouseExited(MouseEvent evt) {
    handleEvent(evt); }
  public void mousePressed(MouseEvent evt) {
    handleEvent(evt); }
  public void mouseReleased(MouseEvent evt) {
    handleEvent(evt); }

  public void keyPressed(KeyEvent evt) {
    handleEvent(evt); }
  public void keyReleased(KeyEvent evt) {
    handleEvent(evt); }

  public void keyTyped(KeyEvent evt) {
    handleEvent(evt); }

  public void focusGained(FocusEvent evt) {
    handleEvent(evt); }
  public void focusLost(FocusEvent evt) {
    handleEvent(evt); }

  public void itemStateChanged(ItemEvent evt) {
    handleEvent(evt); }

  public void mouseDragged (MouseEvent evt) {
    handleEvent(evt); }
  public void mouseMoved(MouseEvent evt) {
    handleEvent(evt); }

  void handleEvent(java.util.EventObject evt) {
    try {
      proc.apply(Environment.top, new Cons(evt)); }
    catch (SchemeException e) {
      System.out.println("Error handling event " + evt + ": " + e);
      e.printBacktrace(Scheme.out);
    }}
}
       
