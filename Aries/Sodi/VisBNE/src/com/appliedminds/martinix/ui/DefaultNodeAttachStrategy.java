package com.appliedminds.martinix.ui;

import java.awt.geom.Rectangle2D;
import java.awt.geom.Point2D;

/**
 * <b>DefaultNodeAttachStrategy</b> represents a strategy for
 * attaching two rectangles at their NORTH, WEST, SOUTH or EAST
 * points.
 *
 * <p>The connection specifications between the two rectangles are
 * adapted from <a
 * href="https://aristotle.apmindsf.com/KnowledgeWeb/286">
 * https://aristotle.apmindsf.com/KnowledgeWeb/286</a>
 *
 * <p><b>The dimensions and coordinates here are in "world",
 * euclidian space.</b>
 *
 * @author daepark@apmindsf.com
 */
public class DefaultNodeAttachStrategy
  implements NodeAttachStrategy
{
  private static final Range RANGE_N_TO_W = new Range(11.25, 45.0);
  private static final Range RANGE_N_TO_S = new Range(45.0, 135.0);
  private static final Range RANGE_N_TO_E = new Range(135.0, 168.75);
  private static final Range RANGE_W_TO_E = new Range(168.75, 191.25);
  private static final Range RANGE_S_TO_E = new Range(191.25, 225.0);
  private static final Range RANGE_S_TO_N = new Range(225.0, 315.0);
  private static final Range RANGE_S_TO_W = new Range(315.0, 348.75);


  /**
   * Find the attach points between two rectangles.
   *
   * <p>The connection specifications between the two rectangles are
   * adapted from <a
   * href="https://aristotle.apmindsf.com/KnowledgeWeb/286">
   * https://aristotle.apmindsf.com/KnowledgeWeb/286</a>
   *
   * <p><b>The dimensions and coordinates here are in "world",
   * euclidian space.</b>
   *
   * @param source the source or tail rectangle in euclidian space.
   * @param target the target or head rectangle in euclidian space.
   * @return an AttachPoints object with source and target points in
   * euclidian space.
   */
  public AttachPoints findAttachPoints(Rectangle2D source,
                                       Rectangle2D target)
  {
    double sourceHalfHeight = source.getHeight() / 2.0;
    double sourceCenterX = source.getCenterX();
    double sourceCenterY = source.getY() - sourceHalfHeight; // this is in world coordinate

    double targetHalfHeight = target.getHeight() / 2.0;
    double targetCenterX = target.getCenterX();
    double targetCenterY = target.getY() - targetHalfHeight; // this is in world coordinate

    // get the slope (rise/run)
    double dy = targetCenterY - sourceCenterY;
    double dx = targetCenterX - sourceCenterX;

    double degrees = Math.toDegrees(Math.atan2(dy, dx));

    if (degrees < 0) {
      degrees += 360;
    }

    int sourceAttachment = -1;
    int targetAttachment = -1;

    if (RANGE_N_TO_W.inRange(degrees, true)) {
      //      System.err.println("NORTH to WEST attachment");
      sourceAttachment = NORTH;
      targetAttachment = WEST;
    }
    else if (RANGE_N_TO_S.inRange(degrees, false)) {
      //      System.err.println("NORTH to SOUTH attachment");
      sourceAttachment = NORTH;
      targetAttachment = SOUTH;
    }
    else if (RANGE_N_TO_E.inRange(degrees, true)) {
      //      System.err.println("NORTH to EAST attachment");
      sourceAttachment = NORTH;
      targetAttachment = EAST;
    }
    else if (RANGE_W_TO_E.inRange(degrees, false)) {
      //      System.err.println("WEST to EAST attachment");
      sourceAttachment = WEST;
      targetAttachment = EAST;
    }
    else if (RANGE_S_TO_E.inRange(degrees, true)) {
      //      System.err.println("SOUTH to EAST attachment");
      sourceAttachment = SOUTH;
      targetAttachment = EAST;
    }
    else if (RANGE_S_TO_N.inRange(degrees, false)) {
      //      System.err.println("SOUTH to NORTH attachment");
      sourceAttachment = SOUTH;
      targetAttachment = NORTH;
    }
    else if (RANGE_S_TO_W.inRange(degrees, true)) {
      //      System.err.println("SOUTH to WEST attachment");
      sourceAttachment = SOUTH;
      targetAttachment = WEST;
    }
    else {
      // RANGE_E_TO_W
      //      System.err.println("EAST to WEST attachment");
      sourceAttachment = EAST;
      targetAttachment = WEST;
    }

    Point2D sourceAttachPoint = new Point2D.Double();
    switch (sourceAttachment) {
    case NORTH:
      sourceAttachPoint.setLocation(source.getCenterX(),
                                    sourceCenterY + sourceHalfHeight);
      break;
    case WEST:
      sourceAttachPoint.setLocation(source.getMinX(), sourceCenterY);
      break;
    case SOUTH:
      sourceAttachPoint.setLocation(source.getCenterX(),
                                    sourceCenterY  - sourceHalfHeight);
      break;
    default:
      // case EAST
      sourceAttachPoint.setLocation(source.getMaxX(), sourceCenterY);
      break;
    }

    Point2D targetAttachPoint = new Point2D.Double();
    switch (targetAttachment) {
    case NORTH:
      targetAttachPoint.setLocation(target.getCenterX(),
                                    targetCenterY + targetHalfHeight);
      break;
    case WEST:
      targetAttachPoint.setLocation(target.getMinX(), targetCenterY);
      break;
    case SOUTH:
      targetAttachPoint.setLocation(target.getCenterX(),
                                    targetCenterY  - targetHalfHeight);
      break;
    default:
      // case EAST
      targetAttachPoint.setLocation(target.getMaxX(), targetCenterY);
      break;
    }

    return (new AttachPoints(sourceAttachPoint, sourceAttachment,
                             targetAttachPoint, targetAttachment));
  }


  /**
   * A range object.
   */
  private static class Range {
    private double __max;
    private double __min;

    public Range(double min, double max) {
      __min = min;
      __max = max;
    }

    /**
     * Check if the specified value is within this range. If
     * inclusive, do an inclusive check otherwise exclusive check.
     */
    public boolean inRange(double value, boolean inclusive) {
      if (inclusive) {
        return (value <= __max && value >= __min);
      }
      else {
        return (value < __max && value > __min);
      }
    }
  }

} // end class "DefaultNodeAttachStrategy"
