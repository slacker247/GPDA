package com.appliedminds.martinix.fader;

import com.appliedminds.martini.DrawableGraphElement;
import com.appliedminds.martini.MartiniError;
import com.appliedminds.martinix.ui.StringUtils;
import com.appliedminds.martinix.ui.TypeInfo;
import java.net.URL;
import java.net.MalformedURLException;


/**
 * In FaderUI context, graph element types may be associated with a
 * URL commonly used to identify the location of the resource
 * describing the graph element. For example, a node labeled
 * &quot;Google&quot; of type &quot;web-site&quot; may point to a URL
 * that associates the node to the site
 * &quot;http://www.google.com&quot;
 *
 * @author daepark@apmindsf.com
 */
public class TypeInfoAdapter extends TypeInfo {

  public static final String P_TYPE_URLS = "typeURLs";

  private String[] _typeURLs;


  public static TypeInfoAdapter getTypeInfoAdapter(DrawableGraphElement elt) {
    //
    // Fix: TypeInfo should be cached instead of creating a new one.
    //
    return (new TypeInfoAdapter(elt));
  }


  /**
   * Parse the P_TYPE_URLS property along with the TypeInfo properties.
   *
   * @see #getTypeInfoAdapter
   */
  protected TypeInfoAdapter(DrawableGraphElement elt) {
    super(elt);

    String typeURLs = elt.getProperty(P_TYPE_URLS);

    if (typeURLs == null) {
      int len = getTypeCount();
      StringBuffer buf = new StringBuffer("(");
      for (int i=0; i<len; i++) {
        buf.append(NULL);

        if ((i+1) < len) {
          buf.append(", ");
        }
      }
      buf.append(")");
      typeURLs = buf.toString();
    }

    _typeURLs = StringUtils.parseStringList(typeURLs);

    if (getTypeCount() != _typeURLs.length) {
      // if we have a valid gml this should not happen
      throw (new MartiniError("A DrawableGraphElement must have the same number of \"typeURLs\" as types"));
    }
  }


  /**
   * Get the URL associated with the type.
   *
   * @param index the index of the typeURL where 0 &lt;= index &lt;
   * getTypeCount();
   * @return the type URL (as String) at the specified index. May
   * return null.
   */
  public URL getTypeURL(int index) {
    String str = _typeURLs[index];
    if (NULL.equals(str)) {
      return (null);
    }

    try {
      return (new URL(str));
    }
    catch (MalformedURLException e) {
      System.err.println("Invalid type URL, " + str + ": " + e);
      return (null);
    }
  }


  /**
   * Set the URL associated with the type.
   *
   * @param index the index of the typeURL where 0 &lt;= index &lt;
   * getTypeCount();
   * @param url the new url.
   */
  public void setTypeURL(int index, URL url) {
    if (url != null) {
      if (!getTypeURL(index).equals(url)) {
        _typeURLs[index] = url.toString();

        // modify url property
        String val = StringUtils.toStringList(_typeURLs);
        getElement().setProperty(P_TYPE_URLS, val);
      }
    }
  }


  /**
   * Add a new type with the associated URL to this TypeInfoAdapter.
   *
   * @param type the new type
   * @param visible the visiblity of the new type.
   * @param url the associated URL.
   */
  public void add(String type, boolean visible, URL url) {
    super.add(type, visible);

    String[] newTypeURLs = new String[_typeURLs.length + 1];

    System.arraycopy(_typeURLs, 0, newTypeURLs, 0, _typeURLs.length);
    newTypeURLs[newTypeURLs.length - 1] = url.toString();

    _typeURLs = newTypeURLs;

    // modify url property
    String val = StringUtils.toStringList(_typeURLs);
    getElement().setProperty(P_TYPE_URLS, val);
  }


  /**
   * Check the url length is also equal to number of types.
   */
  protected void check() {
    super.check();

    if (getTypeCount() != _typeURLs.length) {
      throw (new IllegalStateException("TypeInfo type and url count are not equal"));
    }
  }

} // end class "TypeInfoAdapter"
