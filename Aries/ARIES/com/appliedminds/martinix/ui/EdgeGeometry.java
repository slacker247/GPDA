package com.appliedminds.martinix.ui;

import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;

import com.appliedminds.martini.GraphPanel;
import com.appliedminds.martini.DrawableEdge;
import com.appliedminds.martini.DrawableNode;
import com.appliedminds.martini.Viewport;
import com.appliedminds.martinix.ui.BorderEdgeAttachStrategy;


/**
 * A helper class that encapsulates certain aspects of the edge
 * geometry calculations, like arrowhead generation and coordinate
 * system transformations.
 *
 * <p><i>This has been ported from the vge.lf.dlf.EdgeGeometry class
 * so some code in here may be confusing.  At some point all the old
 * code will be fixed up and this message will be removed.</i>
 *
 * @see also com.appliedminds.martinix.greenpill.GreenPillUI
 * @see also com.appliedminds.martinix.slider.SliderUI
 *
 * @author ben@apmindsf.com
 * @author mathias@apmindsf.com
 * @author daepark@apmindsf.com
 * @author will@apmindsf.com
 */
public class EdgeGeometry
{
  //
  // RIGHT means "in the direction from tail to head".
  // LEFT  means "in the direction from head to tail".
  //
  // Enums do double duty in the geometry calculations, that's why
  // they are floating point types.
  //
  private static final double RIGHT =  1.0;
  private static final double LEFT  = -1.0;

  private static final double HEADNODE_ARROW_OFFSET = 20;

  // Points are stored from head to tail
  private Point2D edgePoints[];


  // Three points that define each arrowhead
  private Point2D arrowHeadRightPts[];
  private Point2D arrowHeadLeftPts[];

  private Point2D bubblePoint;  // the center point of a straight line between the head and tail nodes

  // The overall bounding box for the edge.
  private Rectangle2D bounds;

  private BorderEdgeAttachStrategy _attachStrategy;

  /**
   * This constructor expects the bounds in <b>world</b> coordinates.
   *
   * @param arrowAtHeadNode puts the arrow at the head node (disregard
   * arrowHeadOffSet) if true.
   */
  public EdgeGeometry(DrawableEdge edge,
                      boolean isDirected,
                      Rectangle2D headNodeWorldBounds,
                      Rectangle2D tailNodeWorldBounds,
                      double arrowHeadLength,
                      double arrowHeadFlair,
                      double arrowHeadOffset,
                      boolean arrowAtHeadNode)
  {
    this(edge,
         isDirected,
         headNodeWorldBounds,
         tailNodeWorldBounds,
         arrowHeadLength,
         arrowHeadFlair,
         arrowHeadOffset,
         arrowAtHeadNode,
         new DefaultBorderEdgeAttachStrategy());
  }


  public EdgeGeometry(DrawableEdge edge,
                      boolean isDirected,
                      Rectangle2D headNodeWorldBounds,
                      Rectangle2D tailNodeWorldBounds,
                      double arrowHeadLength,
                      double arrowHeadFlair,
                      double arrowHeadOffset,
                      boolean arrowAtHeadNode,
                      BorderEdgeAttachStrategy attachStrategy)
  {
    _attachStrategy = attachStrategy;

    Point2D headNodeCenter = getWorldBoundsCenter(headNodeWorldBounds);

    Point2D tailNodeCenter = getWorldBoundsCenter(tailNodeWorldBounds);

    // FIX:
    //                // Self edge with no intermediate points.
    //                if ((head == tail) && (intermediatePointCount == 0))
    //                {
    //                    p2vp.x = p1vp.getX() + 1;
    //                    p2vp.y = p1vp.getY();
    //                }

    Point2D headNodeAttachPoint =
      findAttachPoint(headNodeWorldBounds,
                      tailNodeCenter,
                      _attachStrategy.getHeadNodeAttachPoint());

    Point2D tailNodeAttachPoint =
      findAttachPoint(tailNodeWorldBounds,
                      headNodeCenter,
                      _attachStrategy.getTailNodeAttachPoint());

    // Store all the edge points in a single array

    edgePoints = new Point2D.Double[2];
    edgePoints[0] = headNodeAttachPoint;
    edgePoints[1] = tailNodeAttachPoint;

    bubblePoint = new Point2D.Double((edgePoints[0].getX() +
                                      edgePoints[1].getX()) / 2,
                                     (edgePoints[0].getY() +
                                      edgePoints[1].getY()) / 2);

    if (isDirected)
    {
      calcArrowHeadPoints(arrowHeadLength,
                          arrowHeadFlair,
                          arrowHeadOffset,
                          arrowAtHeadNode,
                          edgePoints[1],
                          edgePoints[0],
                          bubblePoint);
    }

    // For use by drawing optizations.
    calcBounds();
  }





  /**
   * Static so it can be called for rubber-banding during
   * edge creation, as well as during normal rendering. We're assuming
   * the points are given in world coordinate system (where the origin
   * is in the center of a plane).
   *
   * @param rect node boundry rectangle.
   * @param pt a point close to the node.
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
    return _attachStrategy.findAttachPoint(rect, pt, preferredAttachPoint);
  }




  /**
   * Transform all the points in one swoop.
   */
  public EdgeGeometry mapWorldToViewport(GraphPanel graphPanel)
  {
    Point2D newEdgePoints[] = new Point2D.Double[edgePoints.length];
    for (int i = 0; i < edgePoints.length; i++)
    {
      newEdgePoints[i] = graphPanel.mapWorldToViewport(edgePoints[i]);
    }

    Point2D newArrowHead1Points[] = new Point2D.Double[arrowHeadRightPts.length];
    for (int i = 0; i < arrowHeadRightPts.length; i++)
    {
      newArrowHead1Points[i] = graphPanel.mapWorldToViewport(arrowHeadRightPts[i]);
    }

    Point2D newArrowHead2Points[] = new Point2D.Double[arrowHeadLeftPts.length];
    for (int i = 0; i < arrowHeadLeftPts.length; i++)
    {
      newArrowHead2Points[i] = graphPanel.mapWorldToViewport(arrowHeadLeftPts[i]);
    }

    Point2D newBubblePoint = graphPanel.mapWorldToViewport(bubblePoint);

    EdgeGeometry newEdgeGeometry = new EdgeGeometry(newEdgePoints,
                                                    newArrowHead1Points,
                                                    newArrowHead2Points,
                                                    newBubblePoint);
    return newEdgeGeometry;
  }


  public Point2D[] getEdgePoints()
  {
    return edgePoints;
  }


  /**
   * RIGHT means in the direction from TAIL to HEAD.
   */
  public Point2D[] getArrowHeadRightPts()
  {
    return arrowHeadRightPts;
  }

  /**
   * LEFT means in the direction from HEAD to TAIL.
   */
  public Point2D[] getArrowHeadLeftPts()
  {
    return arrowHeadLeftPts;
  }


  public Point2D getBubblePoint()
  {
    return (getCenterPoint());
  }

  /**
   * The center point of a straight line connecting the head and tail
   * attach points.
   */
  public Point2D getCenterPoint()
  {
    return (bubblePoint);
  }

  public Point2D getHeadPoint()
  {
    return edgePoints[0];
  }

  public Point2D getTailPoint()
  {
    return edgePoints[1];
  }

  public Rectangle2D getBounds()
  {
    return bounds;
  }


  // Private constructor called only by mapWorldToViewport(). we
  // expect viewport coordinates here
  private EdgeGeometry(Point2D edgePoints[],
                       Point2D arrowHeadRightPts[],
                       Point2D arrowHeadLeftPts[],
                       Point2D bubblePoint)
  {
    this.edgePoints = edgePoints;
    this.arrowHeadRightPts = arrowHeadRightPts;
    this.arrowHeadLeftPts = arrowHeadLeftPts;
    this.bubblePoint = bubblePoint;

    // For use by drawing optizations.
    calcBounds();
  }


  /**
   * @param pt1 equivalent of the "tail" point
   * @param pt2 equivalent the "head" point
   */
  public void calcArrowHeadPoints(double arrowHeadLength,
                                  double arrowHeadFlair,
                                  double arrowHeadOffset,
                                  boolean arrowAtHeadNode,
                                  Point2D pt1,
                                  Point2D pt2,
                                  Point2D bubblePoint)
  {
    // There may be one or two arrow heads near the bubble
    arrowHeadRightPts = new Point2D.Double[3];
    arrowHeadLeftPts = new Point2D.Double[3];

    // Calculate horizontal and vertical components of slope
    double edx = pt2.getX() - pt1.getX();
    double edy = pt2.getY() - pt1.getY();

    double length = Math.sqrt(edx * edx + edy * edy);

    edx /= length;
    edy /= length;

    // Calculate a point arrowHeadOffset units from
    // bubblePoint along the edge pt1 -> pt2
    // remember: RIGHT = tail to head and LEFT = head to tail
    Point2D arrowHeadLeftBasePt;
    Point2D arrowHeadRightBasePt;

    if (arrowAtHeadNode) {
      arrowHeadLeftBasePt =
        new Point2D.Double(pt1.getX() + (HEADNODE_ARROW_OFFSET * edy),
                           pt1.getY() + (HEADNODE_ARROW_OFFSET * edy));
      arrowHeadRightBasePt =
        new Point2D.Double(pt2.getX() - (HEADNODE_ARROW_OFFSET * edx),
                           pt2.getY() - (HEADNODE_ARROW_OFFSET * edy));
    } else {
      arrowHeadLeftBasePt =
        new Point2D.Double((bubblePoint.getX() - (arrowHeadOffset * edx)),
                           (bubblePoint.getY() - (arrowHeadOffset * edy)));

      arrowHeadRightBasePt =
        new Point2D.Double((bubblePoint.getX() + (arrowHeadOffset * edx)),
                           (bubblePoint.getY() + (arrowHeadOffset * edy)));
    }

    // Calculate a point arrowHeadLength units from
    // arrowHeadLeftBasePt along the edge pt1 -> pt2

    Point2D arrowHeadLeftTipPt =
      new Point2D.Double((arrowHeadLeftBasePt.getX() - (arrowHeadLength * edx)),
                         (arrowHeadLeftBasePt.getY() - (arrowHeadLength * edy)));

    Point2D arrowHeadRightTipPt =
      new Point2D.Double((arrowHeadRightBasePt.getX() + (arrowHeadLength*edx)),
                         (arrowHeadRightBasePt.getY() + (arrowHeadLength * edy)));

    // Calculate the points of arrowhead 1
    Point2D a1pt1 =
      new Point2D.Double((arrowHeadLeftBasePt.getX() +
                          (arrowHeadLength * edy * arrowHeadFlair)),
                         (arrowHeadLeftBasePt.getY() -
                          (arrowHeadLength * edx * arrowHeadFlair)));

    Point2D a1pt3 =
      new Point2D.Double((arrowHeadLeftBasePt.getX() -
                          (arrowHeadLength * edy * arrowHeadFlair)),
                         (arrowHeadLeftBasePt.getY() +
                          (arrowHeadLength * edx * arrowHeadFlair)));

    arrowHeadLeftPts[0] = a1pt1;
    arrowHeadLeftPts[1] = new Point2D.Double(arrowHeadLeftTipPt.getX(),
                                             arrowHeadLeftTipPt.getY());
    arrowHeadLeftPts[2] = a1pt3;

    // Calculate the points of arrowhead 2
    Point2D a2pt1 =
      new Point2D.Double((arrowHeadRightBasePt.getX() +
                          (arrowHeadLength * edy * arrowHeadFlair)),
                         (arrowHeadRightBasePt.getY() -
                          (arrowHeadLength * edx * arrowHeadFlair)));

    Point2D a2pt3 =
      new Point2D.Double((arrowHeadRightBasePt.getX() -
                          (arrowHeadLength * edy * arrowHeadFlair)),
                         (arrowHeadRightBasePt.getY() +
                          (arrowHeadLength * edx * arrowHeadFlair)));

    arrowHeadRightPts[0] = a2pt1;
    arrowHeadRightPts[1] = new Point2D.Double(arrowHeadRightTipPt.getX(),
                                              arrowHeadRightTipPt.getY());
    arrowHeadRightPts[2] = a2pt3;
  }


  private void calcBounds()
  {
    double left;
    double top;
    double right;
    double bottom;

    // assert(edgePoints != null);
    // assert(edgePoints.length > 0);

    Point2D p = edgePoints[0];
    double  x = p.getX();
    double  y = p.getY();
    int     i;

    left   = x;
    top    = y;
    right  = x;
    bottom = y;

    for (i = 1; i < edgePoints.length; i++)
    {
      p = edgePoints[i];
      x = p.getX();
      y = p.getY();

      if (x < left)
      {
        left = x;
      }
      else if (x > right)
      {
        right = x;
      }

      if (y < top)
      {
        top = y;
      }
      else if (y > bottom)
      {
        bottom = y;
      }
    }

    if (arrowHeadRightPts != null)
    {
      for (i = 0; i < arrowHeadRightPts.length; i++)
      {
        p = arrowHeadRightPts[i];
        x = p.getX();
        y = p.getY();

        if (x < left)
        {
          left = x;
        }
        else if (x > right)
        {
          right = x;
        }

        if (y < top)
        {
          top = y;
        }
        else if (y > bottom)
        {
          bottom = y;
        }
      }
    }

    if (arrowHeadLeftPts != null)
    {
      for (i = 0; i < arrowHeadLeftPts.length; i++)
      {
        p = arrowHeadLeftPts[i];
        x = p.getX();
        y = p.getY();

        if (x < left)
        {
          left = x;
        }
        else if (x > right)
        {
          right = x;
        }

        if (y < top)
        {
          top = y;
        }
        else if (y > bottom)
        {
          bottom = y;
        }
      }
    }

    bounds = new Rectangle2D.Double(left,
                                    top,
                                    right - left,
                                    bottom - top);
  }


  private Point2D getWorldBoundsCenter(Rectangle2D bounds) {
    double centerX = bounds.getCenterX();
    double centerY = bounds.getY() - (bounds.getHeight() / 2);

    return (new Point2D.Double(centerX, centerY));
  }

} // end class EdgeGeometry
