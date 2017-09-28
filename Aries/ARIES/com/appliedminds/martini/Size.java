package com.appliedminds.martini;

import java.awt.Dimension;



/**
 * Simple class that encapsulates a width and a height.
 */
public class Size
{
  private double _width;
  private double _height;

  public Size() {
    _width = 0;
    _height = 0;
  }

  public Size(double w, double h) {
    _width = w;
    _height = h;
  }

  public Size(Dimension d) {
    _width = d.getWidth();
    _height = d.getHeight();
  }

  public double getWidth() {
    return _width;
  }

  public double getHeight() {
    return _height;
  }

  public void setWidth(double w) {
    _width = w;
  }

  public void setHeight(double h) {
    _height = h;
  }

  public void setSize(double w, double h) {
    _width = w;
    _height = h;
  }

  public String toString() {
    return ("[" + _width + ", " + _height + "]");
  }

  public boolean equals(Object o) {
    Size s = (Size) o;
    return ((s._width == _width) && (s._height == _height));
  }

} // end class Size
