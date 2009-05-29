package org.heavymeta.vehicles;

public class Log
{
    public static final int ERROR = 3;
    public static final int WARN = 2;
    public static final int INFO = 1;
    public static final int DEBUG = 0;

    public static void log(int status, String message)
    {
	StringBuffer msg = new StringBuffer();
	switch (status) {
	case ERROR:
	    msg.append("Error: ");
	    break;
	case WARN:
	    msg.append("Warning: ");
	    break;
	case DEBUG:
	    msg.append("Debug: ");
	    break;
	case INFO:
	    break;
	}
	msg.append(message);
	System.err.println(msg);
    }

    public static void error(String msg)
    {
	log(ERROR, msg);
    }

    public static void info(String msg)
    {
	log(INFO, msg);
    }

    public static void warn(String msg)
    {
	log(WARN, msg);
    }

    public static void debug(String msg)
    {
	log(DEBUG, msg);
    }

}
