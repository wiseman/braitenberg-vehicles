package org.heavymeta.vehicles;

import java.io.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;


import java.util.Date;

public class VehicleTest2 {


    public static void main(String argv[])
    throws com.ibm.jikes.skij.SchemeException
    {
	final World w;
	if (argv.length != 1) {
	    System.err.println("usage: org.heavymeta.vehicles.VehicleTest2 <world file>");
	} else {
	    // Load the world
	    Log.info("Loading " + argv[0]);
	    try {
		w = World.load(new File(argv[0]));
	    }
	    catch (Exception e) {
		Log.error("Could not open the world file \"" + argv[0] + "\" [" + e + "]");
		return;
	    }
	    Log.info("\nRunning...");


	    // Create a new window using the default renderer.
	    VFrame f = new VFrame(argv[0], Standard2DRenderer.class, w);

	    f.addWindowListener(new WindowAdapter() {
		public void windowClosing(WindowEvent e) {
		    System.exit(0);
		}
	    });

	    f.pack();
	    f.setSize(300,300);
	    f.setVisible(true);

	    final Renderer renderer = f.renderer;

	    Timer t = new Timer(10, new ActionListener() {
	        public void actionPerformed(ActionEvent e) {
			w.step();
			renderer.render();
	        }
	        
	    });
	    t.start();
	}

    }
    
}
