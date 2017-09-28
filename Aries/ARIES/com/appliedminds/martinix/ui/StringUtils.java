package com.appliedminds.martinix.ui;

import java.util.StringTokenizer;
import java.util.List;
import java.util.ArrayList;


/**
 * A collection of commonly used String utilities in the martini
 * and martinix packages.
 */
public class StringUtils {

  /**
   * Parse a list represented as string to an array of Strings. A
   * string list looks like &quot;(string one, string two, ...)&quot;.
   *
   * @return an array of Strings representing the string list.
   */
  public static  String[] parseStringList(String stringList) {
    List list = new ArrayList();
    StringTokenizer st = new StringTokenizer(stringList, "(),");
    int sz = st.countTokens();
    for (int i =0; i < sz; ++i) {
      list.add(st.nextToken().trim());
    }

    String[] retVal = new String[list.size()];
    for (int i = 0; i < retVal.length; i++) {
      retVal[i] = (String)list.get(i);
    }

    return (retVal);
  }


  /**
   * @see #parseStringList
   */
  public static  String toStringList(String[] list) {
    StringBuffer buf = new StringBuffer("(");
    for (int i = 0; i < list.length; i++) {
      buf.append(list[i]);

      if ((i+1) < list.length) {
        buf.append(", ");
      }
    }
    buf.append(")");

    return (buf.toString());
  }


  /*
   * Truncate long node labels with ellipses (strings that end in
   * ...).
   *
   * @param text a potentially long string
   * @return the same text, but shortened with an ellipse if the text
   * exceeded some max length.
   */
  public static String ellipsify(String text, int max) {
    if ((text != null) && (text.length() > max)) {
      text = text.substring(0, max - 3) + "...";
    }
    return(text);
  }

}
