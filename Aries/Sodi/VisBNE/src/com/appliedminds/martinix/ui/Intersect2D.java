package com.appliedminds.martinix.ui;

import java.awt.Point;
import java.awt.geom.*;

/**
 * <b>Intersect2D</b> contains various utilities for finding an intersection
 * between two general 2D shapes.
 *
 * @author daepark@apmindsf.com
 */
public class Intersect2D {

  /**
   * Get the first intersecting point of the path and a circle with
   * the given center point and radius. May return null if no
   * intersection. These are all in device (AWT) coordinates.
   *
   * @param path the path in question if it intersects the specified circle.
   * @param center the center of the intersecting circle.
   * @param radius the radius of the intersecting circle.
   */
  public static Point getArcIntersectPoint(GeneralPath path,
                                           Point2D center,
                                           double radius)
  {
    Point xPt = null;

    // the arc
    Ellipse2D arc = new Ellipse2D.Double(center.getX() - radius,
                                         center.getY() - radius,
                                         2 * radius,
                                         2 * radius);
    int count = 0;
    double x1 = 0;
    double y1 = 0;
    double x2 = 0;
    double y2 = 0;

    PathIterator itr = arc.getPathIterator(null, 1.0);
    while (!itr.isDone()) {
      double[] coords = {0, 0, 0, 0, 0, 0};
      int type = itr.currentSegment(coords);

      x1 = coords[0];
      y1 = coords[1];

      if (count > 0) {
        Line2D l = new Line2D.Double(x1, y1, x2, y2);
        if ((xPt = getLineIntersectPoint(path, l)) != null) {
          return (xPt);
        }
      }

      count++;
      x2 = x1;
      y2 = y1;

      itr.next();
    }

    return (null);  // no intersection
  }



  /**
   * Get the first intersecting point of the path and a line. May
   * return null if no intersection. These are all in device (AWT)
   * coordinates.
   *
   * @param path the path in question if it intersects the specified line.
   * @param line the intersecting line.
   */
  public static Point getLineIntersectPoint(GeneralPath path, Line2D line) {
    Point xPt = null;

    int count = 0;
    double x1 = 0;
    double y1 = 0;
    double x2 = 0;
    double y2 = 0;

    PathIterator itr = path.getPathIterator(null, 1.0);
    while (! itr.isDone()) {
      double[] coords = {0, 0, 0, 0, 0, 0};
      int type = itr.currentSegment(coords);

      x1 = coords[0];
      y1 = coords[1];

      if (count > 0) {
        Line2D line2 = new Line2D.Double(x1, y1, x2, y2);
        if ((xPt = getIntersectPoint(line, line2)) != null) {
          return (xPt);
        }
      }

      count++;
      x2 = x1;
      y2 = y1;

      itr.next();
    }

    return (null);
  }


  /**
   * Get the intersecting point of two lines. May return null if no
   * intersection. These are all in device (AWT) coordinates.
   *
   * @param line the first intersecting line.
   * @param line the second intersecting line.
   */
  public static Point getIntersectPoint(Line2D l1, Line2D l2) {

    if (! l1.intersectsLine(l2)) {
      return (null);
    }

    if ((l1.getX1() == l1.getX2()) && (l2.getX1() != l2.getX2 ())) {
      double m2 = (l2.getY1() - l2.getY2()) / (l2.getX1() - l2.getX2());
      double b2 = l2.getY1() - (l2.getX1() * m2);
      double y2 = (m2 * l1.getX1()) + b2;
      Point point = new Point();
      point.setLocation(l1.getX1(), y2);

      return (point);
    }
    else if ((l2.getX1() == l2.getX2()) && (l1.getX1() != l1.getX2())) {
      double m1 = (l1.getY1() - l1.getY2()) / (l1.getX1() - l1.getX2());
      double b1 = l1.getY1() - (l1.getX1() * m1);
      double y1 = (m1 * l2.getX1()) + b1;
      Point point = new Point();
      point.setLocation(l2.getX1(), y1);

      return (point);
    }
    else {
      double m1 = (l1.getY1() - l1.getY2()) / (l1.getX1() - l1.getX2());
      double b1 = l1.getY1() - (l1.getX1() * m1);

      double m2 = (l2.getY1() - l2.getY2()) / (l2.getX1() - l2.getX2());
      double b2 = l2.getY1() - (l2.getX1() * m2);

      double x = (b2 - b1) / (m1 - m2);
      double y1 = (m1 * x) + b1;
      double y2 = (m2 * x) + b2;

      int diff = (int) (y1 - y2);

      if (diff > -1 && diff < 1) {
        Point point = new Point();
        point.setLocation(x, y1);

        return (point);
      }
    }

    return (null); // does not intersect
  }

} // end class "Intersect2D"

