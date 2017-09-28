package com.appliedminds.martini;


import javax.swing.JComponent;
import java.awt.image.BufferedImage;


/** 
 * A buffered component is one that maintains a buffer of its most
 * recently drawn self.
 *
 * <p>This class exists to make it possible to use the PanAndScanPanel
 * with any old component.  This may seem a little hacky, but until a
 * better way is found to get a JComponent to draw in a buffer this
 * will have to do.  It is fairly easy to implement this interface for
 * any component.  For an example see the GraphPanel.
 *
 *
 * @see com.appliedminds.martini.PanAndScanPanel
 *
 * @author mathias@apmindsf.com
 */
public interface BufferedComponent {

  /**
   * Get a reference to the actual JComponent.
   *
   * @return the component.
   */
  JComponent getBufferedComponent();


  /**
   * Get a reference to the components Image.
   *
   * @return the image of the painted component.
   */
  BufferedImage getBufferedComponentImage();

}
