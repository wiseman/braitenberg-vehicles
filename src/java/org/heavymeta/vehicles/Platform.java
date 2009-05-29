package org.heavymeta.vehicles;

import java.util.*;
import java.lang.*;
import java.lang.Math.*;
import java.awt.Color;
import java.io.*;

/** Objects with a position and orientation in space.
 */

public class Platform extends NamedObject implements Serializable {
  public Location m_location = new Location();
  public Orientation m_orientation = new Orientation();
  public Location old_location = new Location();
  public Color color = new Color(0, 0, 0);
	  
	public Location location() {
		return m_location;
	}
	
	public Orientation orientation() {
		return m_orientation;
	}
}
