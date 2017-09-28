package com.appliedminds.martinix.ui;

import java.awt.Composite;
import java.awt.AlphaComposite;
import java.awt.Graphics2D;

/**
 * <b>DrawnObject</b> is the base class for all objects that know how
 * to redraw itself in the same manner.
 *
 * @author daepark@apmindsf.com
 */
public abstract class DrawnObject {

  Composite _composite = null;

  public DrawnObject() {
    this(null);
  }

  /**
   * @param composite the composite used when drawing this object onto
   * a Graphics2D. If null, then use the default Graphics2D composite
   * which is AlphaComposite.SRC_OVER.
   */
  public DrawnObject(Composite composite) {
    _composite = composite;
  }

  /**
   * Redraw itself to the given graphics context.
   *
   * @param graphics the Graphics2D context it should redraw itself to.
   */
  public void redraw(Graphics2D graphics) {
    if (_composite == null) {
      graphics.setComposite(AlphaComposite.SrcOver);
    }
    else {
      graphics.setComposite(_composite);
    }
  }

} // end interface DrawnObject
