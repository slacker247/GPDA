/* -*- Mode: Java; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
package com.appliedminds.martini;

import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.awt.geom.AffineTransform;
import java.awt.geom.NoninvertibleTransformException;


/**
 * The viewport represents the transformation from the DrawableGraphs
 * abstract "world" coordinate space into the concrete "viewport"
 * coordinate space of the screen.
 *
 * <p>World coordinate space is euclidian, with (0,0) in the center,
 * and with Y increasing as you go up, and X increasing as you go
 * left.  Conceptually the world coordinate space is infinite, but we
 * set a bounds to it that is equal to the smallest bounding rectangle
 * in which the graph fits.
 *
 * <P>Viewport coordinate space is screen space with (0,0) in the
 * upper left, Y increasing as you go down, and X increasing as you go
 * right.  The size of the the viewport space is set by the Frame or
 * whatever awt containers is being used.
 *
 * <p>This object supports scaling of the world coordinate space.  A
 * scale value of 1.0 implies no scaling.  A value greater than 1.0
 * increases the size of the graph, values less than 1.0 and greater
 * than 0.0 reduce the size.
 *
 *
 *
 * @author ben@sf.appliedminds.net
 * @author mathias@apmindsf.com
 * @author daepark@apmindsf.com
 * @author will@apmindsf.com
 */
public class Viewport
{

  //
  // The viewport rectangle currently mapped from the world.  Assumes
  // y-axis down computer graphics coordinates, so the rectangle is
  // expressed as [left, top, width, height].
  //
  private Rectangle2D _viewportBounds;


  //
  // The world bounds.  The world coordinate system is really
  // infinite, but this rectangle represents our "area of interest".
  // In terms of the GraphPanel, the whole graph should fit inside
  // this rectangle.
  //
  // This value must be set via the setWorldBounds() method.
  //
  // This rectangle is used to compute the translation step required
  // to map to the viewport bounds.  In essence, the top-left corner
  // of this rectangle must end up at (0,0) in the java viewport
  // coordinates.
  //
  private Rectangle2D _worldBounds;


  // FIX: these do not yet contain the translation portion of the
  // transform. we should fold that in.
  private AffineTransform worldToViewportTransform = null;
  private AffineTransform viewportToWorldTransform = null;


  private double _scale = 1.0;

  // the graph bounds' offset from the origin. *important* this is in
  // world coordinate
  private double _deltaX;
  private double _deltaY;


  /**
   * Default constructor.
   */
  public Viewport() {
    composeViewTransform();
  }


  public void setScale(double s) {
    _scale = s;
    composeViewTransform();
  }



  public double getScale() {
    return(_scale);
  }


  /**
   * Set the size of the world.  This bounds is the size of the graph
   * in world coordinates.  We need to use this value to do a
   * transform when mapping to viewport coordinates.  The top-left
   * corner of this bounds rectangle needs to end up at 0,0 in the
   * viewport coordinates.
   *
   * @param bounds the world bounds.
   */
  public void setWorldBounds(Rectangle2D bounds) {
    if (bounds != null) {
      _worldBounds = bounds;
      _deltaX = _worldBounds.getX();
      _deltaY = _worldBounds.getY();
      composeViewTransform();
    }
  }


  /**
   * Compose a transformation that maps the windowBounds
   * (specified in world coordinates) onto the viewportBounds
   * (specified in viewport coordinates).
   */
  public void composeViewTransform()
  {
    // note that the transforms are applied by Java
    // in the reverse of the order they are composed here
    worldToViewportTransform = new AffineTransform();
    worldToViewportTransform.scale(_scale, _scale);

    try {
      viewportToWorldTransform = worldToViewportTransform.createInverse();
    }
    catch (NoninvertibleTransformException e) {
      throw (new MartiniError(e.getMessage()));
    }
  }


  /**
   * Map a point specified in World coordinates onto
   * a point specified in Viewport coordinates.
   *
   * @param point A point specified in World coordinates.
   * @return A point specified in Viewport coordinates.
   */
  public Point2D mapWorldToViewport(Point2D point)
  {
    double srcPts[] = new double[2];
    double dstPts[] = new double[2];

    srcPts[0] = point.getX();
    srcPts[1] = point.getY();

    Point2D translated = translateWorldToViewport(srcPts[0], srcPts[1]);

    double trPt[] = new double[2];
    trPt[0] = translated.getX();
    trPt[1] = translated.getY();

    worldToViewportTransform.transform(trPt, 0, dstPts, 0, 1);

    return (new Point2D.Double(dstPts[0], dstPts[1]));
  }


  /**
   * Map a rectangle specified in World coordinates onto
   * a rectangle specified in Viewport coordinates.
   *
   * @param rectangle A rectangle specified in World coordinates.
   * @return A rectangle specified in Viewport coordinates.
   */

  public Rectangle2D mapWorldToViewport(Rectangle2D rectangle)
  {
    double srcPts[] = new double[4];
    Point2D dstPts[] = new Point2D[2];

    srcPts[0] = rectangle.getX();
    srcPts[1] = rectangle.getY();
    srcPts[2] = rectangle.getX() + rectangle.getWidth();
    srcPts[3] = rectangle.getY() - rectangle.getHeight();

    dstPts[0] = mapWorldToViewport(new Point2D.Double(srcPts[0],
                                                      srcPts[1]));
    dstPts[1] = mapWorldToViewport(new Point2D.Double(srcPts[2],
                                                      srcPts[3]));

    return new Rectangle2D.Double(dstPts[0].getX(),
                                  dstPts[0].getY(),
                                  dstPts[1].getX() - dstPts[0].getX(),
                                  dstPts[1].getY() - dstPts[0].getY());
  }


  /**
   * Map a point specified in Viewport coordinates onto
   * a point specified in World coordinates.
   *
   * @param point A point specified in Viewport coordinates.
   * @return A point specified in World coordinates.
   */
  public Point2D mapViewportToWorld(Point2D point)
  {
    double srcPts[] = new double[2];
    double dstPts[] = new double[2];

    srcPts[0] = point.getX();
    srcPts[1] = point.getY();

    viewportToWorldTransform.transform(srcPts, 0, dstPts, 0, 1);
    Point2D transformed = new Point2D.Double(dstPts[0], dstPts[1]);

    return (translateViewportToWorld(transformed));
  }


  /**
   * Map a rectangle specified in Viewport coordinates onto
   * a rectangle specified in World coordinates.
   *
   * @param rectangle A rectangle specified in Viewport coordinates.
   * @return A rectangle specified in World coordinates.
   */
  public Rectangle2D mapViewportToWorld(Rectangle2D rectangle) {
    double srcPts[] = new double[4];
    Point2D dstPts[] = new Point2D[2];

    srcPts[0] = rectangle.getX();
    srcPts[1] = rectangle.getY();
    srcPts[2] = rectangle.getX() + rectangle.getWidth();
    srcPts[3] = rectangle.getY() - rectangle.getHeight();

    dstPts[0] = mapViewportToWorld(new Point2D.Double(srcPts[0],
                                                      srcPts[1]));
    dstPts[1] = mapViewportToWorld(new Point2D.Double(srcPts[2],
                                                      srcPts[3]));

    return (new Rectangle2D.Double(dstPts[0].getX(),
                                   dstPts[0].getY(),
                                   dstPts[1].getX() - dstPts[0].getX(),
                                   dstPts[1].getY() - dstPts[0].getY()));
  }



  /**
   * Map a size value from the viewport to the world.  Only scaling
   * effects this transform.
   *
   * @param size the size of something in the viewport coordinates.
   * @return the size of the same thing in the world coordinates.
   */
  public Size mapViewportToWorld(Size size)
  {
    Size res = new Size(mapViewportToWorld(size.getWidth()),
                        mapViewportToWorld(size.getHeight()));
    return(res);
  }


  /**
   * Map a size value from the world to the viewport world.  Only scaling
   * effects this transform.
   *
   * @param size the size of something in the world coordinates.
   * @return the size of the same thing in the viewport coordinates.
   */
  public Size mapWorldToViewport(Size size)
  {
    Size res = new Size(mapWorldToViewport(size.getWidth()),
                        mapWorldToViewport(size.getHeight()));
    return(res);
  }



  /**
   * Map a length specified in World coordinates onto a length
   * specified in Viewport coordinates. This can be used to map things
   * like line thicknesses and font sizes that do not occupy well
   * defined spaces in the World coordinate system.
   *
   * @param width A length specified in World coordinates.
   * @return A length specified in Viewport coordinates.
   */
  public double mapWorldToViewport(double width)
  {
    return (width * _scale);
  }


  /**
   * Map a length specified in Viewport coordinates onto a length
   * specified in World coordinates. This can be used to map things
   * like line thicknesses and font sizes that do not occupy well
   * defined spaces in the World coordinate system.
   *
   * @param width A length specified in World coordinates.
   * @return A length specified in Viewport coordinates.
   */
  public double mapViewportToWorld(double width)
  {
    return (width / _scale);
  }


  /**
   * Perform clipping in world coordinates.
   *
   *
   * @param bounds The position and sizes of a rectangle in world
   * coordinates.
   * @return true if any portion of the rectangle is visible in the
   * window, false otherwise.
   */
  public boolean isVisibleInWindow(Rectangle2D bounds)
  {
    return true;
  }


  /**
   * Perform clipping in viewport coordinates.
   *
   * @param bounds The position and sizes of a rectangle in
   * viewport coordinates.
   * @return true if any portion of the rectangle is visible in the
   * viewport, false otherwise.
   */
  private boolean isVisibleInViewport(Rectangle2D bounds)
  {
    return true;
  }


  /*
   * Translate a point from world to viewport.
   */
  private Point2D translateWorldToViewport(double x, double y) {
    double xv =  x - _deltaX;
    double yv =  -1.0 * (y - _deltaY);

    return(new Point2D.Double(xv, yv));
  }


  /*
   * Translate a point from viewport to world.
   */
  private Point2D translateViewportToWorld(Point2D point) {
    double xw = point.getX() + _deltaX;
    double yw = (-1.0 * point.getY()) + _deltaY;

    return(new Point2D.Double(xw, yw));
  }

} // end class Viewport
