package com.ibm.jikes.skij.misc;

import java.applet.*;
import java.awt.Image;
import java.awt.Graphics;
import java.awt.image.ColorModel;
import java.net.URL;
import java.util.Enumeration;
import sun.applet.AppletResourceLoader;

/**
 * Support class for running applets under Skij. 
 */
public class SkijAppletContext implements AppletContext {

  public AudioClip getAudioClip(URL uRL) {
    return AppletResourceLoader.getAudioClip(uRL);
  }

  public Image getImage(URL uRL) {
    System.out.println("getImage " + uRL);
    return AppletResourceLoader.getImage(uRL);
  }


  public Applet getApplet(String name) {
    return null;
  }

  public Enumeration getApplets() {
    return null;
  }

  public void showDocument(URL url) {
  }
  public void showDocument(URL url, String target) {
  }

  public void showStatus(String status) {
    System.out.println(status);
  }
}

