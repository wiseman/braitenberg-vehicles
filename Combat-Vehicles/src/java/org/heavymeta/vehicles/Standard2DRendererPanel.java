package org.heavymeta.vehicles;

import javax.swing.*;
import javax.swing.event.*;
import java.awt.*;
import java.awt.event.*;


public class Standard2DRendererPanel extends JPanel implements ComponentListener, ItemListener
{
    public Standard2DRenderer renderer;
    public JPanel canvas;
    public JPanel controls;
    
    public Standard2DRendererPanel(Standard2DRenderer renderer)
    {
	Checkbox control;
	
	this.renderer = renderer;
	
	this.setLayout(new BorderLayout());
	
	
	canvas = new Canvas(renderer);
	canvas.setSize(300,250);
	canvas.setLocation(0,0);
	canvas.setDoubleBuffered(false);
	
//	canvas.setBackground(new java.awt.Color(100, 100, 100));
	
	controls = new JPanel();
	control = new Checkbox("Trails", false);
	control.addItemListener(this);
	controls.add(control);
	control = new Checkbox("Sync", false);
	control.addItemListener(this);
	controls.add(control);
	control = new Checkbox("Double Buffer", true);
	control.addItemListener(this);
	controls.add(control);
	
	controls.setLocation(0, 250);
//	controls.setBackground(new java.awt.Color(100, 100, 100));
    }
    
    public void addNotify() {
	int width, height;
	
	System.out.println("Add");
	super.addNotify();
	
	width = getSize().width;
	height = getSize().height;
	
	System.out.println(width);
	System.out.println(height);
	
	canvas.setLocation(0,0);
	canvas.setSize(width, height-50);
	controls.setLocation(0, height-50);
	controls.setSize(width, 50);
	add(canvas, "Center");
	add(controls, "South");
    }	
    
    public void itemStateChanged(ItemEvent e)
    {
	boolean selected = e.getStateChange() == ItemEvent.SELECTED;
	String source = (String) e.getItem();
	
	if (source.equals("Trails")) {
	    renderer.trails = selected;
	}
	if (source.equals("Sync")) {
	    renderer.sync = selected;
	}
	if (source.equals("Double Buffer")) {
	    renderer.double_buffer = selected;
	}
	
    }
    
    
    public void componentHidden(ComponentEvent e)
    {
    }

    public void componentMoved(ComponentEvent e)
    {
    }

    public void componentResized(ComponentEvent e)
    {
	Log.debug("componentResized");
	renderer.resize();
    }

    public void componentShown(ComponentEvent e)
    {
	Log.debug("componentShown");
	renderer.resize();
    }

    


}


 
class Canvas extends JPanel
{
    Standard2DRenderer renderer;
    
    public Canvas(Standard2DRenderer r)
    {
        renderer = r;
    }
    
    public void paintComponent(Graphics g)
    {
	Log.debug("paintComponent");
	
//	super.paintComponent(g);
	renderer.resize();
	renderer.draw_whole_thing = true;
	renderer.render(g);
    }
    
}
