package org.heavymeta.vehicles;

public class SchemeEncoder
{
    static public String encode (String text)
    {
	String s = text;

	s = searchAndReplace(s, "\\", "\\\\");
	s = searchAndReplace(s, "\"", "\\\"");
	return s;
    }
    static  String searchAndReplace(String text, String from, String to)
    {
	StringBuffer sb = new StringBuffer();
	int start, end;
	
	start = 0;
	end = text.indexOf(from, start);
	while (end != -1) {
	    sb.append(text.substring(start, end));
	    sb.append(to);
	    start = end + from.length();
	    end = text.indexOf(from, start);
	}
	if (end == -1) {
	    sb.append(text.substring(start));
	}
	return sb.toString();
    }


    public static void main(String argv[])
    {
	System.out.println(encode(argv[0]));
    }


}
