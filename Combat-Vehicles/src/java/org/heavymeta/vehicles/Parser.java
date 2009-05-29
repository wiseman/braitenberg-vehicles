
package org.heavymeta.vehicles;

import java.io.File;
import java.io.FileNotFoundException;
import com.ibm.jikes.skij.*;

public class Parser {

    static boolean schemeIsInitialized = false;

    public World parseWorldFile(File file)
	throws SchemeException, FileNotFoundException
    {
	if (!schemeIsInitialized) {
	    Scheme.quiet = true;
	    Scheme.evalString("(load-resource \"lib/vehicles.scm\"" +
			      "\"org.heavymeta.vehicles.Parser\")");
	    schemeIsInitialized = true;
	}
	World w;
	Procedure loadProc;
	loadProc = (Procedure) Environment.top.getBinding(Symbol.intern("load-world"));
	try {
	w = (World) loadProc.apply(Environment.top, new Cons(file.toString()));
	return w;
	}
	catch (SchemeException e) {
	    if (e.encapsulated instanceof FileNotFoundException) {
		throw (FileNotFoundException) e.encapsulated;
	    } else {
		throw e;
	    }
	}
    }
}
