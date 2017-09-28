package com.appliedminds.hmv;

import com.appliedminds.martini.DrawableGraphElement;

import java.net.URL;
import java.awt.Point;


/**
 * <b>SupportHandler</b> is an interface to handlers that want to
 * process some arbitrary URL. Usually, a SupportHandler is registered
 * via a SupportHandlerManager associated with a supporting document
 * type.
 *
 * <p>Each SupportHandler should have a null constructor.
 *
 * @author daepark@apmindsf.com
 */
public interface SupportHandler {

  /**
   * Handle or process the given URL.
   *
   * @param element the graph element with the suppporting document.
   * @param url The URL to be processed by the handler. The URL should
   * be recognized or known by the handler.
   * @param clickPoint Usually, a SupportHandler is instantiated and
   * invoked in response to a mouse click event. The clickPoint (in
   * screen coordinates) can be passed onto the handleURL method or
   * null.
   */
  public void handleURL(DrawableGraphElement element,
                        URL url,
                        Point clickPoint);

} // end interface "SupportHandler"
