/* -*- Mode: Java; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
package com.appliedminds.martinix.ui;


import java.awt.Color;

/**
 * <p>A quick and dirty way to support color gradients.
 * Needs to be replaced, refactored, and/or moved
 *
 * <p><i>This has been ported from the vge.core.GraphColor class so
 * some code in here may be confusing.  At some point all the old code
 * will be fixed up and this message will be removed.</i>
 *
 *
 *
 * @author ben@sf.appliedminds.net
 */
public class GraphColor
{
  public static final int SOLID = 0;
  public static final int HORIZONTAL_GRADIENT = 1;
  public static final int VERTICAL_GRADIENT = 2;

  private int style;

  private Color color1;
  private Color color2;

  public GraphColor(Color color)
  {
    this.color1 = color;
    this.style = SOLID;
  }

  public GraphColor(Color color1, Color color2, int style)
  {
    this.color1 = color1;
    this.color2 = color2;
    this.style = style;
  }

  public GraphColor(int r, int g, int b)
  {
    this.color1 = new Color(r, g, b);
    this.style = SOLID;
  }

  public GraphColor(int r1, int g1, int b1, int r2, int g2, int b2, int style)
  {
    this.color1 = new Color(r1, g1, b1);
    this.color2 = new Color(r2, g2, b2);
    this.style = style;
  }

  public Color getColor() { return getColor1(); }
  public Color getColor1() { return color1; }
  public Color getColor2() { return color2; }

  public boolean isHorizontalGradient() { return style == HORIZONTAL_GRADIENT; }
  public boolean isVerticalGradient() { return style == VERTICAL_GRADIENT; }

} // end class GraphColor
