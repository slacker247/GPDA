/* -*- Mode: Java; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
package com.appliedminds.martinix.slider;

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
  private double _strokeWidth;
  private GraphColor _strokeColor;
  private double _strokeOpacity;

  private GraphColor _fillColor;
  private double _fillOpacity;

  private GraphColor _switchColor;
  private GraphColor _prSwitchColor;

  private GraphColor _fontColor;
  private double _fontSize;

  private int _maxLabelWidth;

  public DrawProps(
    double strokeWidth,
    GraphColor strokeColor,
    double strokeOpacity,
    GraphColor fillColor,
    double fillOpacity,
    GraphColor switchColor,
    GraphColor prSwitchColor,
    GraphColor fontColor,
    double fontSize,
    int maxWidth)
  {
    setStrokeWidth(strokeWidth);
    setStrokeColor(strokeColor);
    setStrokeOpacity(strokeOpacity);
    setFillColor(fillColor);
    setFillOpacity(fillOpacity);
    _switchColor = switchColor;
    _prSwitchColor = prSwitchColor;
    setFontColor(fontColor);
    setFontSize(fontSize);
    setMaxLabelWidth(maxWidth);
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
    setMaxLabelWidth(clone.getMaxLabelWidth());
    _switchColor = clone._switchColor;
    _prSwitchColor = clone._prSwitchColor;
  }

  public double getStrokeWidth()
  {
    return _strokeWidth;
  }

  public GraphColor getStrokeColor()
  {
    return _strokeColor;
  }

  public double getStrokeOpacity()
  {
    return _strokeOpacity;
  }

  public GraphColor getFillColor()
  {
    return _fillColor;
  }

  public GraphColor getSwitchColor(boolean prMode)
  {
    return (prMode ? _prSwitchColor : _switchColor);
  }

  public double getFillOpacity()
  {
    return _fillOpacity;
  }

  public GraphColor getFontColor()
  {
    return _fontColor;
  }

  public double getFontSize()
  {
    return _fontSize;
  }

  public int getMaxLabelWidth()
  {
    return _maxLabelWidth;
  }

  public void setStrokeWidth(double strokeWidth)
  {
    _strokeWidth = strokeWidth;
  }

  public void setStrokeColor(GraphColor strokeColor)
  {
    _strokeColor = strokeColor;
  }

  public void setStrokeOpacity(double strokeOpacity)
  {
    _strokeOpacity = strokeOpacity;
  }

  public void setFillColor(GraphColor fillColor)
  {
    _fillColor = fillColor;
  }

  public void setFillOpacity(double fillOpacity)
  {
    _fillOpacity = fillOpacity;
  }

  public void setFontColor(GraphColor fontColor)
  {
    _fontColor = fontColor;
  }

  public void setFontSize(double fontSize)
  {
    _fontSize = fontSize;
  }


  public void setMaxLabelWidth(int w) {
    _maxLabelWidth = w;
  }

} // end class DrawProps
