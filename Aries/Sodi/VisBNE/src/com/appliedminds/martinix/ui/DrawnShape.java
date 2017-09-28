package com.appliedminds.martinix.ui;

import java.awt.Graphics2D;
import java.awt.Shape;
import java.awt.Paint;
import java.awt.Stroke;
import java.awt.Composite;


/**
 * A DrawnObject implementation for a drawn Shape.
 *
 * @author daepark@apmindsf.com
 */
public class DrawnShape extends DrawnObject {

  private Shape  _shape;
  private Paint  _fillPaint;
  private Stroke _stroke;
  private Paint  _strokePaint;


  /**
   * Initialize a DrawnShape.
   *
   * @param shape the drawn shape (required).
   * @param fillPaint the color used for filling this shape. If null,
   * the shape will not be filled when redrawn.
   * @param strokePaint the color used for drawing this shape. If null,
   * the outline of the shape will not be redrawn.
   * @param stroke the stroke used for drawing the outline of this
   * shape. If null, the stroke specified by the Graphics2D object
   * will be used when redrawn.
   */
  public DrawnShape(Shape shape, Paint fillPaint,
                    Paint strokePaint, Stroke stroke)
  {
    super();
    _shape = shape;
    _strokePaint = strokePaint;
    _stroke = stroke;
    _fillPaint = fillPaint;
  }

  public DrawnShape(Shape shape, Paint fillPaint,
                    Paint strokePaint, Stroke stroke, Composite composite)
  {
    super(composite);
    _shape = shape;
    _strokePaint = strokePaint;
    _stroke = stroke;
    _fillPaint = fillPaint;
  }


  /**
   * @return the drawn shape
   */
  public Shape getDrawnShape() {
    return (_shape);
  }


  //
  // begin DrawnObject interface
  //

  public void redraw(Graphics2D graphics) {
    super.redraw(graphics);

    if (_fillPaint != null) {
      graphics.setPaint(_fillPaint);
      graphics.fill(_shape);
    }

    if (_strokePaint != null) {
      if (_stroke != null) {
        graphics.setStroke(_stroke);
      }

      graphics.setPaint(_strokePaint);
      graphics.draw(_shape);
    }
  }

  //
  // end DrawnObject interface
  //

  public String toString() {
    return (_shape.getBounds().toString());
  }

} // end class DrawnShape
