package org.heavymeta.vehicles;

import javax.swing.*;
import java.awt.*;


public class VFrame extends JFrame {
    public Renderer renderer;
    
    public VFrame(String title, Class rendererClass, World world)
    {
	super(title);
	init(rendererClass, world);
    }
    
    /*
    public void paint(Graphics g)
    {
	Log.debug("VFrame paintComponent");
	super.paint(g);
    }
    */
    
    public void init(Class rendererClass, World world)
    {
    	JComponent UIComponent;
    	
	try {
	    try {
	        renderer = (Renderer) rendererClass.newInstance();
	    }
	    catch (InstantiationException e) {
	        Log.error("unable to create renderer \"" + rendererClass + "\"" +
			  " [" + e.toString() + "]");
	    }
	}
	
	catch (IllegalAccessException e) {
	    Log.error("unable to create renderer \"" + rendererClass + "\"" +
	              " [" + e.toString() + "]");
	}
	
	UIComponent = renderer.getUIComponent();
    	UIComponent.setSize(300,300);
    	getContentPane().add(UIComponent);
	renderer.setWorld(world);
    }
    
}

	
	
