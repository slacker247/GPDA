package com.appliedminds.martinix.fader;

import java.net.URL;


/**
 * The context object associated with each fader icon. Specifically,
 * each icon is associated with a type name and an arbitrary URL and its
 * index in the TypeInfo collection of the graph element.
 *
 * @see com.appliedminds.martinix.ui.TypeInfo
 * @author <a href="mailto: daepark@apmindsf.com"</a>
 */
public class FaderIconHitContext {

  private String _type;
  private URL _url;
  private int _index;


  /**
   * @param type the type associated with the icon
   * @param url the url associated with the icon
   * @param index the index into the TypeInfo collection of the
   * graph element that is associted with this context.
   */
  public FaderIconHitContext(String type, URL url, int index) {
    _type = type;
    _url = url;
    _index = index;
  }


  /**
   * Get the type of associated with the icon.
   */
  public String getType() {
    return (_type);
  }


  /**
   * Get the url associated with the icon.
   */
  public URL getURL() {
    return (_url);
  }


  /**
   * Get the index into the TypeInfo collection of the graph element
   * that is associted with this context.
   */
  public int getIndex() {
    return (_index);
  }

} // end class "FaderIconHitContext"
