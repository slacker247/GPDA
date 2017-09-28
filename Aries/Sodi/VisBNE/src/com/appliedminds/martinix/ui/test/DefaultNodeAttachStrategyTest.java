package com.appliedminds.martinix.ui.test;

import java.awt.geom.Rectangle2D;
import java.awt.geom.Point2D;

import junit.framework.*;

import com.appliedminds.martinix.ui.*;

/**
 * Test for com.appliedminds.martinix.ui.DefaultNodeAttachStrategy
 */
public class DefaultNodeAttachStrategyTest extends TestCase {

  /**
   * Source and various target nodes in euclidean coordinate space
   *
   * <pre>
   *              N
   *         NW       NE
   *
   *      W       C       E
   *
   *         SW       SE
   *
   *              S
   * </pre>
   */
  public static final Rectangle2D SOURCE =
    new Rectangle2D.Double(50, 50, 50, 50);

  public static final Rectangle2D TARGET_N =
    new Rectangle2D.Double(50, 100, 50, 50);

  public static final Rectangle2D TARGET_NW =
    new Rectangle2D.Double(25, 75, 50, 50);

  public static final Rectangle2D TARGET_W =
    new Rectangle2D.Double(0, 50, 50, 50);

  public static final Rectangle2D TARGET_SW =
    new Rectangle2D.Double(25, 25, 50, 50);

  public static final Rectangle2D TARGET_S =
    new Rectangle2D.Double(50, 0, 50, 50);

  public static final Rectangle2D TARGET_SE =
    new Rectangle2D.Double(75, 25, 50, 50);

  public static final Rectangle2D TARGET_E =
    new Rectangle2D.Double(100, 50, 50, 50);

  public static final Rectangle2D TARGET_NE =
    new Rectangle2D.Double(75, 75, 50, 50);


  public DefaultNodeAttachStrategyTest(String name) {
    super(name);
  }



  public void testFindAttachPoints() {
    NodeAttachStrategy strategy = new DefaultNodeAttachStrategy();

    NodeAttachStrategy.AttachPoints ap = null;

    //
    // connect north to south
    //
    ap = strategy.findAttachPoints(SOURCE, TARGET_N);
    assertEquals("Source attach point is wrong",
                 new Point2D.Double(SOURCE.getCenterX(),
                                    SOURCE.getY()),  // this is in world coordinate
                 ap.sourcePt);
    assertEquals("Target attach point is wrong",
                 new Point2D.Double(TARGET_N.getCenterX(),
                                    TARGET_N.getY() - TARGET_N.getHeight()),  // this is in world coordinate
                 ap.targetPt);

    //
    // connect north to west
    //
    ap = strategy.findAttachPoints(SOURCE, TARGET_NW);
    assertEquals("Source attach point is wrong",
                 new Point2D.Double(SOURCE.getCenterX(),
                                    SOURCE.getY()),  // this is in world coordinate
                 ap.sourcePt);
    assertEquals("Target attach point is wrong",
                 new Point2D.Double(TARGET_NW.getMaxX(),
                                    TARGET_NW.getY() - TARGET_NW.getHeight() / 2.0),  // this is in world coordinate
                 ap.targetPt);

    //
    // connect west to east
    //
    ap = strategy.findAttachPoints(SOURCE, TARGET_W);
    assertEquals("Source attach point is wrong",
                 new Point2D.Double(SOURCE.getX(),
                                    SOURCE.getY() - SOURCE.getHeight() / 2.0),  // this is in world coordinate
                 ap.sourcePt);
    assertEquals("Target attach point is wrong",
                 new Point2D.Double(TARGET_W.getMaxX(),
                                    TARGET_W.getY() - TARGET_W.getHeight() / 2.0),  // this is in world coordinate
                 ap.targetPt);

    //
    // connect south to east
    //
    ap = strategy.findAttachPoints(SOURCE, TARGET_SW);
    assertEquals("Source attach point is wrong",
                 new Point2D.Double(SOURCE.getCenterX(),
                                    SOURCE.getY() - SOURCE.getHeight()),  // this is in world coordinate
                 ap.sourcePt);
    assertEquals("Target attach point is wrong",
                 new Point2D.Double(TARGET_SW.getMaxX(),
                                    TARGET_SW.getY() - TARGET_SW.getHeight() / 2.0),  // this is in world coordinate
                 ap.targetPt);


    //
    // connect south to north
    //
    ap = strategy.findAttachPoints(SOURCE, TARGET_S);
    assertEquals("Source attach point is wrong",
                 new Point2D.Double(SOURCE.getCenterX(),
                                    SOURCE.getY() - SOURCE.getHeight()),  // this is in world coordinate
                 ap.sourcePt);
    assertEquals("Target attach point is wrong",
                 new Point2D.Double(TARGET_S.getCenterX(),
                                    TARGET_S.getY()),  // this is in world coordinate
                 ap.targetPt);


    //
    // connect south to west
    //
    ap = strategy.findAttachPoints(SOURCE, TARGET_SE);
    assertEquals("Source attach point is wrong",
                 new Point2D.Double(SOURCE.getCenterX(),
                                    SOURCE.getY() - SOURCE.getHeight()),  // this is in world coordinate
                 ap.sourcePt);
    assertEquals("Target attach point is wrong",
                 new Point2D.Double(TARGET_SE.getMinX(),
                                    TARGET_SE.getY() - TARGET_SE.getHeight() / 2.0),  // this is in world coordinate
                 ap.targetPt);


    //
    // connect east to west
    //
    ap = strategy.findAttachPoints(SOURCE, TARGET_E);
    assertEquals("Source attach point is wrong",
                 new Point2D.Double(SOURCE.getMaxX(),
                                    SOURCE.getY() - SOURCE.getHeight() / 2.0),  // this is in world coordinate
                 ap.sourcePt);
    assertEquals("Target attach point is wrong",
                 new Point2D.Double(TARGET_E.getMinX(),
                                    TARGET_E.getY() - TARGET_E.getHeight() / 2.0),  // this is in world coordinate
                 ap.targetPt);

    //
    // connect north to west
    //
    ap = strategy.findAttachPoints(SOURCE, TARGET_NE);
    assertEquals("Source attach point is wrong",
                 new Point2D.Double(SOURCE.getCenterX(),
                                    SOURCE.getY()),  // this is in world coordinate
                 ap.sourcePt);
    assertEquals("Target attach point is wrong",
                 new Point2D.Double(TARGET_NE.getMinX(),
                                    TARGET_NE.getY() - TARGET_NE.getHeight() / 2.0),  // this is in world coordinate
                 ap.targetPt);
  }

}
