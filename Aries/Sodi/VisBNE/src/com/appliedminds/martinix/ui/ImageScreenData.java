package com.appliedminds.martinix.ui;

import java.awt.image.BufferedImage;
import java.awt.geom.Rectangle2D;


/**
 * This object holds information about an image that has been drawn on the
 * screen.
 *
 * <p>The reason that this object is needed is that the BufferedImage
 * object provided by java does not include anything about position.
 *
 * <b>All coordinates and sizes are in screen (ie, viewport)
 * coordinates</b>
 *
 * @author daepark@apmindsf.com
 */
public class ImageScreenData {

	private BufferedImage _image;
	private Rectangle2D _bounds;


	/** 
   * Create a new one with the image and bounds information.
   *
   * @param image the image.
   * @param bounds the drawn bounds including POSITION.
   */
  public ImageScreenData(BufferedImage image, Rectangle2D bounds) {
    _image = image;
    _bounds = bounds;
  }


	/** 
   * Get the drawn image.
   */
	public BufferedImage getBufferedImage() {
		return (_image);
	}

	
	/** 
   * Get the drawn bounds (in screen coordinates).
   */
  public Rectangle2D getBounds() {
    return(_bounds);
  }

} // end class ImageScreenData
