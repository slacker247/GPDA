package com.appliedminds.martinix.ui;


import java.awt.Point;
import java.awt.geom.Point2D;
import java.awt.geom.GeneralPath;
import java.awt.geom.Ellipse2D;
import java.awt.geom.Line2D;
import java.awt.geom.PathIterator;


/**
 * A class that knows how to calculate an arrow head.  This provides
 * access to the arrow head as a series of points.
 *
 *
 * <P><B>Note that all coordinates and dimensions are in world,
 * euclidian coordinates. However, the intersection algorithms are
 * done in device (AWT) or VIEWPORT coordinates</b>
 *
 *
 * @author mathias@apmindsf.com
 * @author will@apmindsf.com
 * @author ben@apmindsf.com
 */
public class ArrowHead {


  private Point2D[] _pts;


  /**
   * Create an arrow head of the specified length and flair with
   * oriented as indiated and centered on the given tip.
   *
   * @param tip the tip of the arrow (in world coordinate space).
   * @param origin a sample point that lies on a line with the tip
   * that is used to determine the orientation of the arrow head (in
   * world coordinate space).
   * @param length the length of the arrow head.
   * @param flair the flair of the arrow head.
   */
  public ArrowHead(Point2D tip, Point2D origin, double length, double flair) {

    _pts = new Point2D[4];

    // Calculate horizontal and vertical components of slope
    double edx = tip.getX() - origin.getX();
    double edy = tip.getY() - origin.getY();

    double len = Math.sqrt(edx * edx + edy * edy);

    edx /= len;
    edy /= len;

    Point2D arrowHeadLeftBasePt =
      new Point2D.Double(tip.getX() - (length * edx),
                         tip.getY() - (length * edy));
    Point2D a1pt4 =
      new Point2D.Double(tip.getX() - ((length - 3) * edx),
                         tip.getY() - ((length - 3) * edy));


    // Calculate the points of arrowhead 1
    Point2D a1pt1 =
      new Point2D.Double((arrowHeadLeftBasePt.getX() + (2 * edy * flair)),
                         (arrowHeadLeftBasePt.getY() - (2 * edx * flair)));

    Point2D a1pt3 =
      new Point2D.Double((arrowHeadLeftBasePt.getX() - (2 * edy * flair)),
                         (arrowHeadLeftBasePt.getY() + (2 * edx * flair)));

    _pts[0] = a1pt1;
    _pts[1] = new Point2D.Double(tip.getX(), tip.getY());
    _pts[2] = a1pt3;
    _pts[3] = a1pt4;
  }


  /**
   * Get the number of points that define the arrow head.
   *
   * @return the number of points that define the arrow head.
   */
  public int getPointCount() {
    return(_pts.length);
  }


  /**
   * Get the point at the specified index.  Indecies run from zero to
   * (getPointCount() - 1).
   *
   * @param index the index into the arrow-head point list.
   * @return the point at that index.
   * @throws ArrayIndexOutOfBoundsException if the index is out of range.
   *
   * @see #getPointCount for the valid indexes.
   */
  public Point2D getPoint(int index) {
    return(_pts[index]);
  }


} // end class "ArrowHead"
