package org.heavymeta.vehicles;

import java.util.*;
import java.io.*;


public class IsAHierarchy implements Serializable {
	public Hashtable children = new Hashtable();
	public Hashtable parents = new Hashtable();
	
	public boolean isA(Object spec, Object abs) {
		if (spec.equals(abs)) {
			return true;
		} else {
			Vector p = (Vector) parents.get(spec);
			if (p == null) {
				return false;
			} else {
				Enumeration elements = p.elements();
				
				while (elements.hasMoreElements()) {
					if (this.isA(elements.nextElement(), abs)) {
						return true;
					}
				}
				
				return false;
			}
		}
	}
	
	public void addChild(String parent, String child) {
		if (!(this.isA(child, parent)) && !(this.isA(parent, child))) {
			Vector parents_vector = (Vector) parents.get(child);
			Vector children_vector = (Vector) children.get(parent);
			
			if (parents_vector == null) {
				parents_vector = new Vector();
			}
			if (children_vector == null) {
				children_vector = new Vector();
			}
			
			parents_vector.addElement(parent);
			children_vector.addElement(child);
			
			parents.put(child, parents_vector);
			children.put(parent, children_vector);
		}
	}
	
	public void addParent(String child, String parent) {
		this.addChild(parent, child);
	}
	
	public void clear() {
		children = new Hashtable();
		parents = new Hashtable();
	}
		
}		

		
				
