package com.ibm.jikes.skij;
import java.net.*;

public class SkijTest {
  public static void main(String args[]) throws Exception {
    Scheme.start();
    Scheme.loadURL(new URL(args[0]));
  }
  

}
