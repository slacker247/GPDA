package com.appliedminds.martini;

import java.awt.geom.Point2D;


/** 
 * An interface to something that presents a dynamic viewport.
 * Callers can use this interface to move the viewport's top left
 * position.
 *
 * <p>This makes a distinction between viewport coordinates and child
 * component coordinates.  Viewport coordinates are those that are in
 * effect in the viewport, and the top left is always (0,0).  Child
 * component coordiantes are the coordinates of the component that is
 * being displayed in the viewport.
 *
 *
 *
 * @author mathias@apmindsf.com
 */
public interface DynamicViewport {


  /**
   * Move the viewport so that the specified child component
   * coordinate is displayed in the upper left hand corner (ie, mapped
   * to (0,0) in viewport coordiantes).
   *
   * @param pt the point (in the child component coordinate system)
   * that should appear in the upper left corner of the viewport.
   */
  void setViewportPosition(Point2D pt);

}
