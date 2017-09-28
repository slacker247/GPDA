package com.appliedminds.martini;

import java.awt.Component;

/**
 * <b>MarqueeContainer</b> knows where and how its MarqueePane is
 * positioned.
 *
 * @author daepark@apmindsf.com
 */
public interface MarqueeContainer {

  /**
   * Get the component that contains the MarqueePane which is most
   * likely a JLayeredPane. This components coordinate space is
   * translated directly to the MarqueePane.
   */
  public Component getMarqueeLayer();

} // end interface "MarqueeContainer"
