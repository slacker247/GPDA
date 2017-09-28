/* -*- Mode: Java; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
package com.appliedminds.martinix.greenpill;

import com.appliedminds.martinix.ui.GraphColor;


/**
 * <p>DrawProps is a container for all the properties
 * needed to draw a particular element of a graph - things
 * like line styles, colors, fonts, etcetera.
 *
 * <p>The units are arbitrary "world coordinate system" units,
 * <b>not</b> device units.
 *
 *
 *
 * <p><i>This has been ported from the vge.core.DrawProps class so
 * some code in here may be confusing.  At some point all the old code
 * will be fixed up and this message will be removed.</i>
 *
 *
 * @author ben@sf.appliedminds.net
 * @author mathias@apmindsf.com
 * @author daepark@apmindsf.com
 */
public class DrawProps
{
  private double strokeWidth_;
  private GraphColor strokeColor_;
  private double strokeOpacity_;

  private GraphColor fillColor_;
  private double fillOpacity_;

  private GraphColor fontColor_;
  private double fontSize_;


  public DrawProps(double strokeWidth,
                   GraphColor strokeColor,
                   double strokeOpacity,
                   GraphColor fillColor,
                   double fillOpacity,
                   GraphColor fontColor,
                   double fontSize)
  {
    setStrokeWidth(strokeWidth);
    setStrokeColor(strokeColor);
    setStrokeOpacity(strokeOpacity);
    setFillColor(fillColor);
    setFillOpacity(fillOpacity);
    setFontColor(fontColor);
    setFontSize(fontSize);
  }


  public DrawProps(DrawProps clone)
  {
    setStrokeWidth(clone.getStrokeWidth());
    setStrokeColor(clone.getStrokeColor());
    setStrokeOpacity(clone.getStrokeOpacity());
    setFillColor(clone.getFillColor());
    setFillOpacity(clone.getFillOpacity());
    setFontColor(clone.getFontColor());
    setFontSize(clone.getFontSize());
  }

  public double getStrokeWidth()
  {
    return strokeWidth_;
  }

  public GraphColor getStrokeColor()
  {
    return strokeColor_;
  }

  public double getStrokeOpacity()
  {
    return strokeOpacity_;
  }

  public GraphColor getFillColor()
  {
    return fillColor_;
  }

  public double getFillOpacity()
  {
    return fillOpacity_;
  }

  public GraphColor getFontColor()
  {
    return fontColor_;
  }

  public double getFontSize()
  {
    return fontSize_;
  }

  public void setStrokeWidth(double strokeWidth)
  {
    strokeWidth_ = strokeWidth;
  }

  public void setStrokeColor(GraphColor strokeColor)
  {
    strokeColor_ = strokeColor;
  }

  public void setStrokeOpacity(double strokeOpacity)
  {
    strokeOpacity_ = strokeOpacity;
  }

  public void setFillColor(GraphColor fillColor)
  {
    fillColor_ = fillColor;
  }

  public void setFillOpacity(double fillOpacity)
  {
    fillOpacity_ = fillOpacity;
  }

  public void setFontColor(GraphColor fontColor)
  {
    fontColor_ = fontColor;
  }

  public void setFontSize(double fontSize)
  {
    fontSize_ = fontSize;
  }

} // end class DrawProps
