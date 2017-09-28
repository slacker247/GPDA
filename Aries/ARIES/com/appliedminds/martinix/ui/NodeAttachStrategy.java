package com.appliedminds.martinix.ui;

import java.awt.geom.Rectangle2D;
import java.awt.geom.Point2D;

import javax.swing.SwingConstants;

/**
 * <b>NodeAttachStrategy</b> is an interface to a strategy that knows
 * how to find attachment points between two rectangles.
 *
 * @author daepark@apmindsf.com
 */
public interface NodeAttachStrategy extends SwingConstants {

  public AttachPoints findAttachPoints(Rectangle2D source,
                                       Rectangle2D target);

  /**
   * Contains a source and target attach point.
   */
  public static class AttachPoints {

    public Point2D sourcePt = new Point2D.Double();
    public Point2D targetPt = new Point2D.Double();
    public int sourceAttachment = -1;
    public int targetAttachment = -1;

    public AttachPoints() { }

    public AttachPoints(Point2D source, int sourceAttachment,
                        Point2D target, int targetAttachment)
    {
      sourcePt.setLocation(source.getX(), source.getY());
      targetPt.setLocation(target.getX(), target.getY());

      checkAttachment(sourceAttachment);
      checkAttachment(targetAttachment);

      this.sourceAttachment = sourceAttachment;
      this.targetAttachment = targetAttachment;
    }

    public String toString() {
      return ("AttachPoints [sourtPt: " + sourcePt +
              ", targetPt: " + targetPt);
    }

    private void checkAttachment(int attachment) {
      switch(attachment) {
      case NORTH:
        return;
      case WEST:
        return;
      case SOUTH:
        return;
      case EAST:
        return;
      default:
        throw (new RuntimeException("Invalid attachment: " + attachment));
      }
    }

  }

} // end interface "NodeAttachPointStrategy"
