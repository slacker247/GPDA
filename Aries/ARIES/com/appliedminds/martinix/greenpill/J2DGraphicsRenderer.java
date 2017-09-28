package com.appliedminds.martinix.greenpill;

import com.appliedminds.martinix.ui.*;

import javax.swing.*;

import java.awt.*;
import java.awt.image.BufferedImage;
import java.awt.image.LookupOp;
import java.awt.image.ByteLookupTable;

import java.awt.geom.*;
import java.awt.font.*;
import java.awt.event.*;

import java.io.*;
import java.net.*;

import java.util.*;



/**
 * This class knows how to draw graphics primitives into a Java
 * Graphics2D object.
 *
 * <p><i>This has been ported from the vge.ot.j2d package so some code
 * in here may be confusing.  At some point all the old code will be
 * fixed up and this message will be removed.</i>
 *
 *
 * @author ben@sf.appliedminds.dnet
 * @author mathias@apmindsf.com
 * @author daepark@apmindsf.com
 */

public class J2DGraphicsRenderer {


  private FontCache _defaultFontCache;



  /**
   * @throws GreenPillUIError if there is a problem initializing the font
   * subsystem.
   */
  public J2DGraphicsRenderer(String fontResourcePath) throws GreenPillUIError
  {
    _defaultFontCache = new FontCache(fontResourcePath);
  }


  public DrawnShape drawRoundedRectangle(Graphics2D graphics,
                                          DrawProps drawProps,
                                          Rectangle2D bounds,
                                          Point2D arcDimensions)
  {
    RoundRectangle2D rect = new RoundRectangle2D.Double(
      bounds.getX(),
      bounds.getY(),
      bounds.getWidth(),
      bounds.getHeight(),
      arcDimensions.getX(),
      arcDimensions.getY());

    Paint fillPaint = makePaint(drawProps.getFillColor(), bounds);
    Stroke stroke = new BasicStroke((float)drawProps.getStrokeWidth());
    Paint strokePaint = makePaint(drawProps.getStrokeColor(), bounds);
    DrawnShape drawnShape = new DrawnShape(rect, fillPaint,
                                           strokePaint, stroke);
    drawnShape.redraw(graphics);
    return (drawnShape);
  }


  public DrawnShape drawEllipse(Graphics2D graphics,
                               DrawProps drawProps,
                               Rectangle2D bounds)
  {
    Ellipse2D ellipse = new Ellipse2D.Double(
      bounds.getX(),
      bounds.getY(),
      bounds.getWidth(),
      bounds.getHeight());

    Paint fillPaint = makePaint(drawProps.getFillColor(), bounds);
    Stroke stroke = new BasicStroke((float)drawProps.getStrokeWidth());
    Paint strokePaint = makePaint(drawProps.getStrokeColor(), bounds);
    DrawnShape drawnShape = new DrawnShape(ellipse, fillPaint,
                                           strokePaint, stroke);
    drawnShape.redraw(graphics);

    return (drawnShape);
  }


  public DrawnShape drawPolyline(Graphics2D graphics,
                                  DrawProps drawProps,
                                  Point2D points[])
  {
    Rectangle2D bounds = new Rectangle2D.Double();
    GeneralPath path = makePath(points, bounds, false);

    Stroke stroke = new BasicStroke((float)drawProps.getStrokeWidth(),
                                    BasicStroke.CAP_ROUND,
                                    BasicStroke.JOIN_ROUND);
    Paint strokePaint = makePaint(drawProps.getStrokeColor(), bounds);

    DrawnShape drawnShape = new DrawnShape(path, null, strokePaint, stroke);
    drawnShape.redraw(graphics);

    return (drawnShape);
  }


  public DrawnShape drawPolygon(Graphics2D graphics,
                                 DrawProps drawProps,
                                 Point2D points[])
  {

    Rectangle2D bounds = new Rectangle2D.Double();
    GeneralPath path = makePath(points, bounds, true);

    Paint fillPaint = makePaint(drawProps.getFillColor(), bounds);
    Stroke stroke = new BasicStroke((float)drawProps.getStrokeWidth(),
                                    BasicStroke.CAP_ROUND,
                                    BasicStroke.JOIN_ROUND);
    Paint strokePaint = makePaint(drawProps.getStrokeColor(), bounds);

    DrawnShape drawnShape = new DrawnShape(path, fillPaint,
                                           strokePaint, stroke);
    drawnShape.redraw(graphics);

    return (drawnShape);
  }




  /**
   * @return a valid TextScreenData object or NULL if the text is null.
   */
  public DrawnText drawText(Graphics2D graphics,
                            DrawProps drawProps,
                            String text,
                            Point2D center)
  {
    if (text == null)
      return(null); // BAIL

    Font font = _defaultFontCache.getFontBySize((int)drawProps.getFontSize());
    FontRenderContext frc = graphics.getFontRenderContext();

    TextLayout layout;
    Rectangle2D bounds;
    Rectangle2D drawnBounds;

    Paint fontPaint = null;

    if (text.length() > 0)
    {
      layout = new TextLayout(text, font, frc);
      bounds = layout.getBounds();

      fontPaint = makePaint(drawProps.getFontColor(), bounds);
      graphics.setPaint(fontPaint);

      //       layout.draw(
      //         graphics,
      //         (float)(center.getX() - (bounds.getWidth() / 2)),
      //         (float)(center.getY() + (layout.getAscent() / 2)));

      // layout.getBounds() gives dimensions, but not position
      drawnBounds =
        new Rectangle2D.Double(center.getX() - (bounds.getWidth() / 2),
                               center.getY() - (layout.getAscent() / 2),
                               bounds.getWidth(),
                               bounds.getHeight());

    }
    else
    {
      // This degenerate case is hacked for in-place
      // text editing.

      // Java TextLayout object cannot be instantiated
      // with an empty String, so we will draw
      // two spaces and pretend like the caret is in
      // between them. This will give us access to a
      // caret shape of the appropriate size, even
      // whene there is no text to be drawn.

      layout = new TextLayout("  ", font, frc);
      bounds = layout.getBounds();

      // We don't actually draw anything.

      // Zero-width
      drawnBounds =
        new Rectangle2D.Double(center.getX(),
                               center.getY() - (layout.getAscent() / 2),
                               0,
                               bounds.getHeight());
    }

    TextScreenData data = new TextScreenData(layout, drawnBounds, text);
    DrawnText drawnText = new DrawnText(data, fontPaint, center);
    drawnText.redraw(graphics);

    return (drawnText);
  }




  public Rectangle2D getTextBounds(Graphics2D graphics,
                                   DrawProps drawProps,
                                   String text)
  {
    Font font = _defaultFontCache.getFontBySize((int)drawProps.getFontSize());
    FontMetrics fm = graphics.getFontMetrics(font);
    double width = fm.stringWidth(text);
    double height = fm.getHeight();
    return new Rectangle2D.Double(0, 0, width, height);
  }



  public DrawnImage drawImage(Graphics2D graphics,
                               BufferedImage img, Point2D pt, double scale)
  {
    Rectangle2D bounds = new Rectangle2D.Double(pt.getX(),
                                                pt.getY(),
                                                (img.getWidth() * scale),
                                                (img.getHeight() * scale));

    ImageScreenData data = new ImageScreenData(img, bounds);
    DrawnImage drawnImage = new DrawnImage(data, pt, scale);
    drawnImage.redraw(graphics);

    return (drawnImage);
  }


  /**
   * @param bounds the drawn bounds of the entire text block.
   */
  public void drawCaret(Graphics2D g,
                        Rectangle2D bounds,
                        TextLayout layout,
                        Shape caret,
      Color c)
  {
    double dx = bounds.getX();
    double dy = bounds.getY() + layout.getAscent();

    g.translate(dx, dy);
    g.setXORMode(c);
    g.draw(caret);
    g.setPaintMode();
    g.translate(-dx, -dy);
  }


  /**
   * @param bounds the drawn bounds of the entire text block.
   */
  public void drawTextSelection(Graphics2D g,
                                Rectangle2D bounds,
                                TextLayout layout,
                                Shape selection,
                                Color c)
  {
    double dx = bounds.getX();
    double dy = bounds.getY() + layout.getAscent();

    g.translate(dx, dy);
    g.setXORMode(c);
    g.fill(selection);
    g.setPaintMode();
    g.translate(-dx, -dy);
  }



  private GeneralPath makePath(Point2D points[], Rectangle2D bounds, boolean closePath)
  {
    GeneralPath path = new GeneralPath();

    double minx = 0;
    double miny = 0;
    double maxx = 0;
    double maxy = 0;

    double x;
    double y;

    for (int i = 0; i < points.length; i++)
    {
      x = points[i].getX();
      y = points[i].getY();

      if (i == 0)
      {
        path.moveTo(
          (float)x,
          (float)y);

        minx = x;
        miny = y;
        maxx = x;
        maxy = y;
      }
      else
      {
        path.lineTo(
          (float)x,
          (float)y);

        if (x < minx)
        {
          minx = x;
        }
        if (y < miny)
        {
          miny = y;
        }
        if (x > maxx)
        {
          maxx = x;
        }
        if (y > maxy)
        {
          maxy = y;
        }
      }
    }

    if (closePath)
    {
      path.closePath();
    }

    bounds.setRect(
      minx,
      miny,
      maxx - minx,
      maxy - miny);

    return path;
  }

  // cheesy hack to support color gradients
  private Paint makePaint(GraphColor graphColor, Rectangle2D bounds)
  {
    Paint result = null;

    if (graphColor.isHorizontalGradient())
    {
      Color color1 = graphColor.getColor1();
      Color color2 = graphColor.getColor2();
      result = new GradientPaint(
        new Point2D.Double(bounds.getX(), bounds.getY()),
        color1,
        new Point2D.Double(bounds.getX() + bounds.getWidth(), bounds.getY()),
        color2);
    }
    else if (graphColor.isVerticalGradient())
    {
      Color color1 = graphColor.getColor1();
      Color color2 = graphColor.getColor2();
      result = new GradientPaint(
        new Point2D.Double(bounds.getX(), bounds.getY()),
        color1,
        new Point2D.Double(bounds.getX(), bounds.getY() + bounds.getHeight()),
        color2);
    }
    else
    {
      result = graphColor.getColor();
    }

    return result;
  }

} // end class J2DGraphicsRenderer
