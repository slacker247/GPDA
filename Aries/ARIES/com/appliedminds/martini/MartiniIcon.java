package com.appliedminds.martini;

import java.awt.Color;
import java.awt.Component;
import java.awt.image.ImageObserver;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.MediaTracker;
import java.awt.Toolkit;
import java.awt.image.BufferedImage;
import java.awt.image.ImageObserver;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;


/**
 * MartiniIcon is an image that can be displayed in a GraphPanel.
 *
 * @author mathias@apmindsf.com
 * @author will@apmindsf.com
 */
public class MartiniIcon {

  private BufferedImage _img;
  private int _width;
  private int _height;



  /**
   * Get a debug string representation of this object.
   */
  public String toString() {
    StringBuffer buf = new StringBuffer();
    buf.append(super.toString());
    buf.append(" { ").append(_width).append("x").append(_height);
    buf.append(" }");
    return(buf.toString());
  }



  /**
   * Load an icon using a URL.
   *
   * @param u a URL pointing to an icon file.
   * @param comp the component used for MediaTracker (optional! Can be
   * null).
   * @return a new MartiniIcon instance.
   */
  public static MartiniIcon load(URL u, Component comp)
    throws MalformedURLException, IOException
  {
    Image img = Toolkit.getDefaultToolkit().getImage(u);
    if (comp != null) {
      MediaTracker tracker = new MediaTracker(comp);
      try {
        tracker.addImage(img, 0);
        tracker.waitForID(0);
      } catch (Exception e) {
        e.printStackTrace();
      }
      if (tracker.isErrorAny()) {
        throw(new IOException("Unable to load icon: " + u));
      }
    }
    else {
      DisposableImageObserver obs = new DisposableImageObserver();
      if (!Toolkit.getDefaultToolkit().prepareImage(img, -1, -1, obs)) {
        obs.waitForIt();
        if (obs.isErrorAny()) {
          throw(new IOException("Unable to load icon: " + u));
        }
      }
    }

    int width = img.getWidth(comp);
    int height = img.getHeight(comp);
    BufferedImage bimg = new BufferedImage(width,
                                           height,
                                           BufferedImage.TYPE_INT_ARGB);
    Graphics2D biContext = bimg.createGraphics();
    biContext.drawImage(img, 0, 0, null);

    return(new MartiniIcon(bimg));
  }


  /**
   * Loads an icon using a path into the resource package
   *
   * @param resourcePath the path to the icon file
   * @return a new MartiniIcon instance
   */
  public static MartiniIcon load(String resourcePath) 
    throws MalformedURLException, IOException
  {
    URL url = null;
    url = ClassLoader.getSystemClassLoader().getResource(resourcePath);
    return MartiniIcon.load(url, null);
  }


  /**
   * Create a new MartiniIcon from the given image.
   *
   * @param img the image data
   */
  public MartiniIcon(BufferedImage img) {
    _img = img;
    _width = _img.getWidth();
    _height = _img.getHeight();
  }


  public BufferedImage getImage() {
    //
    // We may want to return a copy here to prevent accidental
    // modification.
    //
    return(_img);
  }


  public int getHeight() {
    return(_height);
  }


  public int getWidth() {
    return(_width);
  }


  /**
   * @param desiredHeight the desired height of the icon
   * @return the scaling factor that must be applied to this
   * icon to make it the desired height.  A value of 1.0 means
   * no scaling.
   */
  public double getScaleFactor(double desiredHeight) {
    double dheight = _height * 1.0;
    if (Math.abs(desiredHeight - dheight) < 0.0001) {
      return(1.0);
    }
    else {
      return(desiredHeight / (dheight));
    }
  }



  /**
   * Get the center point of the image in the X dimension.
   *
   * @return the X component of the center point.
   */
  public int getCenterX() {
    return(_width / 2);
  }


  /**
   * Get the center point of the image in the Y dimension.
   *
   * @return the Y component of the center point.
   */
  public int getCenterY() {
    return(_height / 2);
  }



  /*
   * Our own ImageObserver that takes care of dealing with Javas
   * asynchronous image loading mechanism.  This is a use-once
   * thing, hence the name.
   *
   * To use it, just pass it along to the toolkits "prepareImage"
   * method and then call waitForIt().  When waitForIt() returns, the
   * image will either be loaded or an error will have occured.  Use
   * isErrorAny() to check for errors.
   *
   * @author mathias@apmindsf.com
   */
  private static class DisposableImageObserver implements ImageObserver {

    private boolean __done;
    private boolean __error;

    public DisposableImageObserver() {
      __done = false;
      __error = false;
    }

    /**
     * @return true if further updates are needed.  False if we are
     * done.
     */
    public boolean imageUpdate(Image img,
                               int infoflags,
                               int x,
                               int y,
                               int width,
                               int height)
    {
      if ((infoflags & ImageObserver.ERROR) != 0) {
        __error = true;
      }



      if ((infoflags & (ALLBITS|ABORT)) == 0) {
        return true;
      }
      else {
        __done = true;
        return false;
      }
    }


    public void waitForIt() {
      try {
        while(!__done) {
          Thread.sleep(100);
        }
      }
      catch(InterruptedException e) {
        __error = true;
      }
    }

    public boolean isErrorAny() {
      return(__error);
    }
  }// end private class "DisposableImageObserver"



} // end class MartiniIcon
