package com.ibm.jikes.skij.misc;
import  com.ibm.jikes.skij.*;
import java.awt.event.*;
import java.util.Vector;

/*
 * @(#)Graph.java	1.3 96/12/06
 *
 * Copyright (c) 1994-1996 Sun Microsystems, Inc. All Rights Reserved.
 *
 * Sun grants you ("Licensee") a non-exclusive, royalty free, license to use,
 * modify and redistribute this software in source and binary code form,
 * provided that i) this copyright notice and license appear on all copies of
 * the software; and ii) Licensee does not utilize the software in a manner
 * which is disparaging to Sun.
 *
 * This software is provided "AS IS," without a warranty of any kind. ALL
 * EXPRESS OR IMPLIED CONDITIONS, REPRESENTATIONS AND WARRANTIES, INCLUDING ANY
 * IMPLIED WARRANTY OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE OR
 * NON-INFRINGEMENT, ARE HEREBY EXCLUDED. SUN AND ITS LICENSORS SHALL NOT BE
 * LIABLE FOR ANY DAMAGES SUFFERED BY LICENSEE AS A RESULT OF USING, MODIFYING
 * OR DISTRIBUTING THE SOFTWARE OR ITS DERIVATIVES. IN NO EVENT WILL SUN OR ITS
 * LICENSORS BE LIABLE FOR ANY LOST REVENUE, PROFIT OR DATA, OR FOR DIRECT,
 * INDIRECT, SPECIAL, CONSEQUENTIAL, INCIDENTAL OR PUNITIVE DAMAGES, HOWEVER
 * CAUSED AND REGARDLESS OF THE THEORY OF LIABILITY, ARISING OUT OF THE USE OF
 * OR INABILITY TO USE SOFTWARE, EVEN IF SUN HAS BEEN ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGES.
 *
 * This software is not designed or intended for use in on-line control of
 * aircraft, air traffic, aircraft navigation or aircraft communications; or in
 * the design, construction, operation or maintenance of any nuclear
 * facility. Licensee represents and warrants that it will not use or
 * redistribute the Software for such purposes.
 */

/*
 * Heavily modified to support Skij's Graph Browser -- Mike Travers, IBM Watson.
 */

import java.util.*;
import java.awt.*;
import java.applet.Applet;  // not used maybe we hope

class Node {
  public double x;
  public double y;

  double dx;
  double dy;

  boolean fixed;

  public String lbl;
  public Object object;
  public Color color;
  
}

class Edge {

  public Node from;
  public Node to;
  public String label;
  public double len = 10;

  Edge(Node f, Node t, String l) {
    from = f; to = t; label = l;
  }

}

/** 
 * A class to support the graphical browser.
 */
public class GraphPanel extends DBPanel implements Runnable {

  public Vector nodes = new Vector();

  public Vector edges = new Vector();

  Thread relaxer;
  public boolean random;

  public GraphPanel(Procedure p) {
    super(p);
  }

  public Node addNode(String lbl, Object obj) {
    Node n = new Node();
    n.x = 10 + 380*Math.random();
    n.y = 10 + 380*Math.random();
    n.lbl = lbl;
    n.object = obj;
    nodes.addElement(n);
    return n;
  }
  
  // +++ because Node isn't (can't be) public, we can't use reflection on it...argh
  public Color getNodeColor(Node n) {
    return n.color;
  }
  public void setNodeColor(Node n, Color c) {
    n.color = c;
  }

  public void deleteNode(Node node) {
    nodes.removeElement(node);
    for (Enumeration enum = edges.elements(); enum.hasMoreElements(); ) {
      Edge e = (Edge)enum.nextElement();
      if (e.from == node || e.to == node) 
	edges.removeElement(e);
    }
  }

  public Node addNode(String lbl, Object obj, Color color) {
    Node n = addNode(lbl, obj);
    n.color = color;
    return n;
  }

  public Edge addEdge(Node from, Node to, String label) {
    Edge e = new Edge(from, to, label);
    edges.addElement(e);
    return e;
  }
  
  public Object nodeObject(Node node) {
    return node.object;
  }

  public void setSpread(double nlen) {
    for (Enumeration e = edges.elements(); e.hasMoreElements(); ) {
      Edge edge = (Edge)e.nextElement();
      edge.len = nlen;
    }
  }
       
  public void run() {
    while (true) {
      relax();
      if (random && (Math.random() < 0.03)) {
	Node n = (Node)nodes.elementAt((int)(Math.random() * nodes.size()));
	if (!n.fixed) {
	  n.x += 100*Math.random() - 50;
	  n.y += 100*Math.random() - 50;
	}
      }
      try {
	Thread.sleep(100);
      } catch (InterruptedException e) {
	break;
      }
    }
  }

  synchronized void relax() {
    for (Enumeration enum = edges.elements(); enum.hasMoreElements(); ) {
      Edge e = (Edge)enum.nextElement();
      double vx = e.to.x - e.from.x;
      double vy = e.to.y - e.from.y;
      double len = Math.sqrt(vx * vx + vy * vy);
      double f = (e.len - len) / (len * 3) ;
      double dx = f * vx;
      double dy = f * vy;

      e.to.dx += dx;
      e.to.dy += dy;
      e.from.dx += -dx;
      e.from.dy += -dy;
    }


    for (Enumeration e = nodes.elements(); e.hasMoreElements(); ) {
      Node n1 = (Node)e.nextElement();
      double dx = 0;
      double dy = 0;

      for (Enumeration e2 = nodes.elements(); e2.hasMoreElements(); ) {
	Node n2 = (Node)e2.nextElement();

	if (n1 == n2) {
	  continue;
	}
	double vx = n1.x - n2.x;
	double vy = n1.y - n2.y;
	double len = vx * vx + vy * vy;
	if (len == 0) {
	  dx += Math.random();
	  dy += Math.random();
	} else if (len < 100*100) {
	  dx += vx / len;
	  dy += vy / len;
	}
      }
      double dlen = dx * dx + dy * dy;
      if (dlen > 0) {
	dlen = Math.sqrt(dlen) / 2;
	n1.dx += dx / dlen;
	n1.dy += dy / dlen;
      }
    }

    Dimension d = size();
    for (Enumeration e = nodes.elements(); e.hasMoreElements(); ) {
      Node n = (Node)e.nextElement();
      if (!n.fixed) {
	n.x += Math.max(-5, Math.min(5, n.dx));
	n.y += Math.max(-5, Math.min(5, n.dy));
	//System.out.println("v= " + n.dx + "," + n.dy);
	if (n.x < 0) {
	  n.x = 0;
	} else if (n.x > d.width) {
	  n.x = d.width;
	}
	if (n.y < 0) {
	  n.y = 0;
	} else if (n.y > d.height) {
	  n.y = d.height;
	}
      }
      n.dx /= 2;
      n.dy /= 2;
    }
    repaint();
  }

  Node pick;
  boolean pickfixed;
  Image offscreen;
  Dimension offscreensize;

  public Color backgroundColor = new Color(60, 60, 60);
  final Color nodeColor = new Color(250, 220, 100);
  public Color labelColor = Color.white;
  public Color edgeColor = new Color(255, 100, 50);

  public void paintNode(Graphics g, Node n, FontMetrics fm) {
    int x = (int)n.x;
    int y = (int)n.y;
    g.setColor(n.color == null ? nodeColor : n.color);
    int w = fm.stringWidth(n.lbl) + 10;
    int h = fm.getHeight() + 4;
    g.fillRect(x - w/2, y - h / 2, w, h);
    g.setColor(Color.black);
    g.drawRect(x - w/2, y - h / 2, w-1, h-1);
    g.drawString(n.lbl, x - (w-10)/2, (y - (h-4)/2) + fm.getAscent());
  }

  // override standard so background isn't cleared
  public void update(Graphics g) {
    paint(g);
  }

  public void paintNodes(Graphics g) {
    for (Enumeration enum = edges.elements(); enum.hasMoreElements(); ) {
      Edge e = (Edge)enum.nextElement();
      int x1 = (int)e.from.x;
      int y1 = (int)e.from.y;
      int x2 = (int)e.to.x;
      int y2 = (int)e.to.y;
      int len = (int)Math.abs(Math.sqrt((x1-x2)*(x1-x2) + (y1-y2)*(y1-y2)) - e.len);
      g.setColor(edgeColor);
      g.drawLine(x1, y1, x2, y2);
	String lbl = e.label;
	g.setColor(labelColor);
	g.drawString(lbl, x2 - (x2-x1)/3, y2 - (y2-y1)/3);
    }

    FontMetrics fm = g.getFontMetrics();
    for (Enumeration e = nodes.elements(); e.hasMoreElements(); ) {
      paintNode(g, (Node)e.nextElement(), fm);
    }
  }

  public Node closestNode(int x, int y) {
    Node p = null;
    double bestdist = Double.MAX_VALUE;
    for (Enumeration e = nodes.elements(); e.hasMoreElements(); ) {
      Node n = (Node)e.nextElement();
      double dist = (n.x - x) * (n.x - x) + (n.y - y) * (n.y - y);
      if (dist < bestdist) {
	p = n;
	bestdist = dist;
      }
    }
    return p;
  }

  public void moveNode(Node n, int x, int y) {
    n.x = x;
    n.y = y;
    repaint();
  }

  public void start() {
    relaxer = new Thread(this);
    relaxer.start();
  }
  public void stop() {
    relaxer.stop();
  }
}

