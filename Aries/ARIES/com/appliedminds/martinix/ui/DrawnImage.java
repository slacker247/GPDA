package com.appliedminds.martinix.ui;

import java.awt.Composite;
import java.awt.Graphics2D;
import java.awt.Shape;
import java.awt.geom.AffineTransform;
import java.awt.geom.Point2D;
import java.awt.image.BufferedImage;

import com.appliedminds.martinix.ui.ImageScreenData;

/**
 * A DrawnObject implementation for a drawn image.
 *
 * <p>The getDrawnObject will return a
 * com.appliedminds.martinix.ui.ImageScreenData.
 *
 * @author daepark@apmindsf.com
 */
public class DrawnImage extends DrawnObject {

  private ImageScreenData _data;
  private Point2D         _point;
  private double          _scale;


  /**
   * Initialize with params need for redrawing an image.
   *
   * @param data the ImageScreenData.
   * @param point the position of the drawn image.
   * will be redrawn.
   * @param scale the scale factor for the image.
   */
  public DrawnImage(ImageScreenData data,
                    Point2D point, double scale)
  {
    super();
    _data = data;
    _point = point;
    _scale = scale;
  }

  public DrawnImage(ImageScreenData data,
                    Point2D point, double scale, Composite composite)
  {
    super(composite);
    _data = data;
    _point = point;
    _scale = scale;
  }


  /**
   * @return the drawn ImageScreenData.
   */
  public ImageScreenData getDrawnImageScreenData() {
    return (_data);
  }


  //
  // begin DrawnObject interface
  //


  public void redraw(Graphics2D graphics) {
    super.redraw(graphics);

     // Move the "pen"
    graphics.translate((int) _point.getX(), (int) _point.getY());

    // Compose the scaling transform
    AffineTransform trans = new AffineTransform();
    trans.setToScale(_scale, _scale);

    // Draw the picture
    graphics.drawRenderedImage(_data.getBufferedImage(), trans);

    // move the "pen" back
    graphics.translate(- (int) _point.getX(), - (int) _point.getY());
  }

  //
  // end DrawnObject interface
  //

} //end class DrawnText
