package com.appliedminds.martinix.ui;


import java.awt.geom.Rectangle2D;
import java.awt.geom.Point2D;


/**
 * This class represents a strategy for attaching edges to nodes by
 * attaching the edge to one of the four "borders" of the nodes
 * bounding rectangle.
 *
 * <p><b>The dimensions and coordinates here are in "world",
 * euclidian space.</b>
 *
 *
 * @author mathias@apmindsf.com
 */
public abstract class BorderEdgeAttachStrategy {

  /**
   * Attach strategy point representing the top most coordinate of
   * the node
   */
  public static final int NORTH    = 1;

  /**
   * Attach strategy point representing the bottom most coordinate of
   * the node
   */
  public static final int SOUTH    = 2;

  /**
   * Attach strategy point representing the left most coordinate of
   * the node
   */
  public static final int WEST     = 3;

  /**
   * Attach strategy point representing the right most coordinate of
   * the node
   */
  public static final int EAST     = 4;

  /**
   * Attach strategy point representing NORTH or SOUTH attach points,
   * depending on the position of the originating point, If the point
   * is from below, then always attach on the SOUTH. If the point is
   * from above, then always attach on the NORTH.
   */
  public static final int Y_AXIS = 5;

  /**
   * Attach strategy point representing WEST or EAST attach points,
   * depending on the position of the originating point, If the point
   * is from the left, then always attach on the WEST. If the point is
   * from the right, then always attach on the EAST.
   */
  public static final int X_AXIS = 6;

  /**
   * Attach strategy point representing the closest NORTH, SOUTH,
   * WEST, EAST point of the node to its connected counter part's
   * attach point.
   */
  public static final int RELATIVE = 7;


  /**
  * The attach-edge-point formula uses this angle value to determine
  * when to attach an edge to the east or west border of a node.
  * This value is centered on 90 degrees.  A value of 50 means that
  * any edge going off to the left or the right inside an angle of 80
  * degrees ( 90 - 50 = 40, 40 * 2 = 80) will be attached to the side
  * of the node instead of the top or bottom.
  *
  * A value of 70 means that the angle for E-W connection is only
  * 40 degrees wide.
  */
  public abstract double getEastWestAngle();


  /**
   * Get the preferred attach point of the tail node.
   */
  public abstract int getTailNodeAttachPoint();


  /**
   * Get the preferred attach point of the head node.
   */
  public abstract int getHeadNodeAttachPoint();


  /**
   * Locate the appropriate attach point given a bounding box for a
   * node and a point indicating where the edge is originating.  The
   * attach point will always be in the center of one of the four
   * sides of the node bounding rectangle.
   *
   * <p><b>The dimensions and coordinates here are in "world",
   * euclidian space.</b>
   *
   * @param rect node boundry rectangle in euclidian space.
   * @param pt a point indicating where the edge is comming from or
   * going to (in euclidian space).
   * @param preferredAttachPoint the preferred attach point
   * (BorderEdgeAttachStrategy.NORTH, BorderEdgeAttachStrategy.SOUTH,
   * BorderEdgeAttachStrategy.WEST, BorderEdgeAttachStrategy.EAST,
   * BorderEdgeAttachStrategy.RELATIVE).
   *
   * @return the point on the node boundry that is the place that a line
   * from the given point should be drawn to.
   */
  public Point2D findAttachPoint(Rectangle2D rect,
                                 Point2D pt,
                                 int preferredAttachPoint)
  {
    double halfHeight = rect.getHeight() / 2.0;
    double centerY = rect.getY() - halfHeight; // this is in world coordinate
    double centerX = rect.getCenterX();

    Point2D ptNorth = new Point2D.Double(centerX, rect.getY());
    Point2D ptSouth = new Point2D.Double(centerX, rect.getY()-rect.getHeight());
    Point2D ptEast  = new Point2D.Double(rect.getX()+rect.getWidth(), centerY);
    Point2D ptWest  = new Point2D.Double(rect.getX(), centerY);

    switch (preferredAttachPoint) {
    case Y_AXIS:
      if (pt.getY() > centerY) {
        return (ptNorth);
      }
      else {
        return (ptSouth);
      }
    case X_AXIS:
      if (pt.getX() > centerX) {
        return (ptEast);
      }
      else {
        return (ptWest);
      }
    case NORTH:
      return (ptNorth);
    case SOUTH:
      return (ptSouth);
    case WEST:
      return (ptWest);
    case EAST:
      return (ptEast);
    default:
      break;
    }

    Point2D attachPoint = null;

    //
    // Next if the point is to the east or west of the node, then
    // we check the angle between a horizontal line and the point.
    // If the angle falls within a certain range, then we go ahead
    // and attach the edge to the sides (either east or west) of
    // the node.
    //
    // When the edge point is below or above the node, we always
    // attach to the corresponding north or south border of the node.
    //
    if (pt.getX() > ptEast.getX()) {
      // if pt lies to the east of the rect, check the angle

      double dy = pt.getY() - ptEast.getY();
      double dx = pt.getX() - ptEast.getX();

      double tanA = dx/dy;
      double A = Math.toDegrees(Math.atan(tanA));


      if (Math.abs(A) > getEastWestAngle()) {
        attachPoint = ptEast;
      }
      else if (A > 0) {
        attachPoint = ptNorth;
      }
      else {
        attachPoint = ptSouth;
      }
    }
    else if (pt.getX() < ptWest.getX()) {
      // if pt lies to the west of the rect, check the angle

      double dy = pt.getY() - ptWest.getY();
      double dx = ptWest.getX() - pt.getX();

      double tanA = dx/dy;
      double A = Math.toDegrees(Math.atan(tanA));

      if (Math.abs(A) > getEastWestAngle()) {
        attachPoint = ptWest;
      }
      else if (A > 0) {
        attachPoint = ptNorth;
      }
      else {
        attachPoint = ptSouth;
      }
    }
    else if (ptNorth.distance(pt) > ptSouth.distance(pt)) {
      attachPoint = ptSouth;
    }
    else {
      attachPoint = ptNorth;
    }

    return attachPoint;
  }

} // end class BorderEdgeAttachStrategy

