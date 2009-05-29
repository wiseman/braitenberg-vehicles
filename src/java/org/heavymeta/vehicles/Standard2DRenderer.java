package org.heavymeta.vehicles;

import java.awt.*;
import java.awt.image.*;
import java.awt.event.*;
import java.lang.*;
import java.util.*;
import javax.swing.*;



public class Standard2DRenderer implements Renderer
{
    // ----- Settings
    public double scale = 20;
    public java.awt.Color background_color = new java.awt.Color(102, 102, 204);
    public boolean trails = false;
    public boolean sync = false;
    public boolean double_buffer = true;

    // ----- The most basic things we need to render
    World world;
    Standard2DRendererPanel panel;
    Image buffer_image;
    Graphics buffer_graphics;
    
    int width;
    int height;
    double origin_x = 0.0;
    double origin_y = 0.0;
    int width2;
    int height2;
    double sorigin_x;
    double sorigin_y;
    
    // ----- Bounding box management for fast rendering
    int inv_bb_ul_x = 0;
    int inv_bb_ul_y = 0;
    int inv_bb_lr_x = 0;
    int inv_bb_lr_y = 0;
    
    int inv_bb[][] = new int[4][50];
    int inv_bb_count = 0;
    
    public boolean draw_whole_thing = true;
    
    
    public String getName()
    {
	return "Standard 2D Renderer";
    }

    public String getDescription()
    {
	return "A simple 2D graphics renderer.";
    }

    public boolean hasSettingsDialog()
    {
	return false;
    }

    public void showSettingsDialog(JFrame f)
    {
    }

    public void setWorld(World w)
    {
	world = w;
	if (panel != null && world != null) {
	    render();
	}
    }

    public JComponent getUIComponent()
    {
	panel = new Standard2DRendererPanel(this);
	return panel;
    }

    public void render()
    {
	Graphics g = panel.canvas.getGraphics();
	if (g != null) {
	    renderInternal(world, g);
	}
    }

    public void render(Graphics g)
    {
        renderInternal(world, g);
    }

    public void sync()
    {
	panel.canvas.getToolkit().sync();
    }

    public void finish()
    {
    }

    public void cancel()
    {
    }


    public void invalidateAll()
    {
	synchronized (this) {
	    update_inv_bb(0, 0, panel.getWidth(), panel.getHeight());
	}
    }




    public void resize()
    {
	Dimension size = panel.canvas.getSize();
  	width = size.width;
  	height = size.height;
  	buffer_image = panel.canvas.createImage(width, height);
  	buffer_graphics = buffer_image.getGraphics();
	draw_whole_thing = true;
	update_inv_bb(0, 0, width, height);

  	width2 = width / 2;
  	height2 = height / 2;
  	sorigin_x = scale * origin_x;
  	sorigin_y = scale * origin_y;
    }
    
    public void setOrigin(double x, double y)
    {
	origin_x = x;
	origin_y = y;
  	sorigin_x = scale * origin_x;
  	sorigin_y = scale * origin_y;
	if (!(panel == null)) {
	    panel.canvas.repaint();
	}
    }  
    
    public void setScale(double scale)
    {
	this.scale = scale;
  	sorigin_x = scale * origin_x;
  	sorigin_y = scale * origin_y;
	if (!(panel == null)) {
	    panel.canvas.repaint();
	}
    }
    

    // ----- Graphics functions

    public void renderInternal(World w, Graphics out)
    {
	Enumeration e;
	TwoWheeledVehicle v;
	Lamp l;
	double x, y, theta;
	Graphics gr;
	Graphics g = out;
	
	if (double_buffer) {
	    gr = buffer_graphics;
	} else {
	    gr = g;
	}

	if (!trails) {
	    draw_inv_bb(gr);
	    begin_update();
	}
	
	e = w.lamps.elements();
	while (e.hasMoreElements()) {
	    renderLamp(gr, ((Lamp) e.nextElement()));
	}
	
	e = w.vehicles.elements();
	while (e.hasMoreElements()) {
	    renderVehicle(gr, ((TwoWheeledVehicle) e.nextElement()));
	}
	
	if (double_buffer) {
	    if (draw_whole_thing) {
		g.setClip(0, 0, width, height);
		draw_whole_thing = false;
	    } else {
		g.setClip(inv_bb_ul_x, inv_bb_ul_y,
			  inv_bb_lr_x - inv_bb_ul_x,
			  inv_bb_lr_y - inv_bb_ul_y);
	    }

	    g.drawImage(buffer_image, 0, 0, panel.canvas);
	}
	
	if (sync) {
	    this.sync();
	}
    }
    
    public void renderLamp(Graphics g, Lamp l)
    {
	Location loc;
	int x, y, w;
	float brightness;
	float cr, cg, cb;
	
	java.awt.Color color;
	
	loc = l.location();
	x = xform_x(loc.x);
	y = xform_y(loc.y);
	w = (int) (0.2 * scale * 2);
	brightness = (float) Math.min(1.0, l.brightness());
	
	color = l.color;
	cr = (float)(color.getRed() / 255.0);
	cg = (float)(color.getGreen() / 255.0);
	cb = (float)(color.getBlue() / 255.0);
	
	color = new java.awt.Color(cr * brightness, cg * brightness,
				   cb * brightness);
	g.setColor(color);
	g.fillOval(x, y, w, w);
	g.setColor(java.awt.Color.black);
	g.drawOval(x, y, w, w);
	
	update_inv_bb(x, y, x + w, y + w);
    }
    
    public void renderVehicle(Graphics g, TwoWheeledVehicle v)
    {
	Location loc;
	int x, y, r, r2, length;
	double theta;
	
	loc = v.location();
	x = xform_x(loc.x);
	y = xform_y(loc.y);
	theta = v.orientation().theta;
	r = (int) (v.wheel_base * scale);
	r2 = r / 2;
	length = (int) (v.length * scale);
	
	g.setColor(v.color);
	g.fillOval(x-r2, y-r2, r, r);
	g.setColor(java.awt.Color.black);
	g.drawOval(x-r2, y-r2, r, r);
	g.drawLine(x, y,
		   ((int) (x + (Math.cos(theta) * length))),
		   ((int) (y - (Math.sin(theta) * length))));
	
	update_inv_bb(x - length, y - length, x + length, y + length);

    }
    

    public int xform_x(double x)
    {
	double offset = width2 - sorigin_x;
	return (int) ((x * scale) + offset);
    }
    
    public int xform_y(double y)
    {
	double offset = height2 - sorigin_y;
	return height - ((int) ((y * scale) + offset));
    }
    


    // ----- Invalid bounding box management
    void begin_update()
    {
	synchronized (this) {
	    inv_bb_ul_x = width;
	    inv_bb_ul_y = height;
	    inv_bb_lr_x = 0;
	    inv_bb_lr_y = 0;
	    inv_bb_count = 0;
	}
    }
    
    public void draw_inv_bb(Graphics g)
    {
	    synchronized (this) {
	        g.setColor(background_color);
	        if (draw_whole_thing) {
		    g.fillRect(0, 0, width, height);
	        } else {
		    for (int i = 0; i < inv_bb_count; i++) {
		        g.fillRect(inv_bb[0][i], inv_bb[1][i],
			           inv_bb[2][i], inv_bb[3][i]);
		    }
	        }
	    }
    }
    
    void update_inv_bb(int x1, int y1, int x2, int y2)
    {
	synchronized (this) {
	    int w = x2 - x1;
	    int h = y2 - y1;
	    
	    if (x1 < inv_bb_ul_x)
		inv_bb_ul_x = x1;
	    if (y1 < inv_bb_ul_y)
		inv_bb_ul_y = y1;
	    if (x2 > inv_bb_lr_x)
		inv_bb_lr_x = x2;
	    if (y2 > inv_bb_lr_y)
		inv_bb_lr_y = y2;
	    
	    inv_bb[0][inv_bb_count] = x1;
	    inv_bb[1][inv_bb_count] = y1;
	    inv_bb[2][inv_bb_count] = w;
	    inv_bb[3][inv_bb_count] = h;
	    inv_bb_count++;
	}
    }
}




