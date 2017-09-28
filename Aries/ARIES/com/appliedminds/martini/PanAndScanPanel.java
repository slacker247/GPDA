package com.appliedminds.martini;


import java.awt.Color;
import java.awt.Dimension;
import java.awt.geom.Rectangle2D;
import java.awt.geom.Point2D;
import javax.swing.JPanel;
import javax.swing.JComponent;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.event.MouseMotionListener;
import java.awt.event.MouseListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.AlphaComposite;
import java.awt.GridLayout;
import java.awt.BasicStroke;
import java.awt.image.BufferedImage;
import java.awt.RenderingHints;
import java.util.Iterator;
import java.awt.geom.AffineTransform;



/**
 * A panel that provides a pan and scan interface to another
 * component.
 *
 *
 * @see com.appliedminds.martinix.ViewportPandAndScanAdapter
 *
 * @author mathias@apmindsf.com
 */
public class PanAndScanPanel extends JPanel {



  private static final int _NAVSIZE = 30;



  private Color _color;
  private Color _borderColor;
  private NavRect _navrect;
  private Point2D _lastPress;
  private boolean _handleDrag;

  private BasicStroke _stroke = new BasicStroke();


  // width & height of this component
  private int _width;
  private int _height;

  // width & height of the map (the small graph)
  private double _mapWidth;
  private double _mapHeight;

  private DynamicViewport _dviewport;
  private BufferedComponent _child;

  private BufferedImage _buffer;
  private boolean _dirty;





  /**
   * Create a new PanAndScanPanel.
   *
   * @param w desired width of the panel.
   * @param h desired height of the panel.
   * @param screenColor the color of the transparent screen used to
   * veil the parts of the graph that are not in the viewport.
   * @param borderColor the border color for the little nav window.
   */
  public PanAndScanPanel(int w,
                         int h,
                         BufferedComponent c,
                         Color screenColor,
                         Color borderColor)

  {
    super(false);

    setLayout(new GridLayout(1,1));

    _width = w;
    _height = h;

    _mapWidth = w;
    _mapHeight = h;

    _color = screenColor;
    _borderColor = borderColor;

    _navrect = new NavRect(0.0, 0.0, _NAVSIZE, _NAVSIZE);
    _lastPress = new Point2D.Double(0.0, 0.0);
    _handleDrag = false;

    _child = c;
    setSize(_width, _height);
    setVisible(true);

    _dirty = true;

    addMouseMotionListener
      (new MouseMotionListener() {
          public void mouseMoved(MouseEvent e) { }
          public void mouseDragged(MouseEvent e) {
            if (_handleDrag) {
              double x = _navrect.getX() +
                (e.getPoint().getX() - _lastPress.getX());

              double y = _navrect.getY() +
                (e.getPoint().getY() - _lastPress.getY());


              boolean moved = _navrect.moveTo(x, y);

              _lastPress.setLocation(e.getPoint());

              if (moved) {
                updateTargetPane();
                _dirty = true;
                repaint();
              }
            }
          }
        });

    addMouseListener
      (new MouseAdapter() {
          public void mousePressed(MouseEvent e) {
            _lastPress.setLocation(e.getPoint());
            _handleDrag = _navrect.contains(_lastPress);
          }
          public void mouseReleased(MouseEvent e) {
            if (_handleDrag) {
              _handleDrag = false;
            }
          }
        });
  }



  /**
   * Set the viewport.  The DynamicViewport is used to scroll the main
   * viewport (in response to the user moving the little nav box
   * around).
   *
   * @param g the scroller callback object.
   */
  public void setDynamicViewport(DynamicViewport v) {
    _dviewport = v;
  }


  /**
   * Set the viewport position by specifying the coordinate of the
   * upper left hand corner in component coordinates.
   *
   * @param wpt the viewport position in WORLD coordinates.
   */
  public void updateViewportPosition(Point2D wpt) {
    if (!_handleDrag) {
      Point2D mypt = mapComponentToViewport(wpt);
      boolean moved = _navrect.moveTo(mypt);
      if (moved) {
        _dirty = true;
        repaint();
      }
    }
  }


  /**
   * Update the size of the navigation box using the given factors.
   * The factors represent the fraction of the component space that is
   * viewable in the navigation box.
   *
   * @param wfactor represents the fraction of the full component
   * width that is visible in the navigation box.
   * @param hfactor represents the fraction of the full component
   * height that is visible in the navigation box.
   */
  public void updateNavBoxFactors(double wfactor, double hfactor) {
    if (wfactor > 1.0) {
      wfactor = 1.0;
    }

    if (hfactor > 1.0) {
      hfactor = 1.0;
    }

    double desiredWidth = _mapWidth * wfactor;
    double desiredHeight = _mapHeight * hfactor;

    _navrect.resize(desiredWidth, desiredHeight);

    _dirty = true;
    repaint();
  }




  public void paintComponent(Graphics g) {
    if (_buffer == null) {
      //
      // We keep a copy of the final rendered image of this component.
      // If dirty is false then we just dump this to the screen and
      // avoid the time needed for a full draw.
      //
      _buffer = (BufferedImage) createImage(_width, _height);
    }

    if (_dirty) {
      // Draw the child component...
      Graphics2D g2 = (Graphics2D) _buffer.getGraphics();

      g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                          RenderingHints.VALUE_ANTIALIAS_ON);

      double sx = (_mapWidth * 1.0) / (_child.getBufferedComponent().getWidth() * 1.0);
      double sy = (_mapHeight * 1.0) / (_child.getBufferedComponent().getHeight() * 1.0);
      BufferedImage img = _child.getBufferedComponentImage();
      if (img != null) {
        g2.drawImage(img, AffineTransform.getScaleInstance(sx, sy), this);
      }

      //
      // Now draw the shaded part and the viewport on top of everything.
      //
      g2.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER,
                                                 0.6f));
      g2.setColor(_color);

      int w = _width;
      int h = _height;

      // TOP
      g2.fill(new Rectangle2D.Double(0,
                                     0,
                                     w,
                                     _navrect.getY()));

      // BOTTOM
      g2.fill(new Rectangle2D.Double(0,
                                     _navrect.getY() + _navrect.getHeight(),
                                     w,
                                     h - (_navrect.getY() + _navrect.getHeight())));

      // LEFT
      g2.fill(new Rectangle2D.Double(0,
                                     _navrect.getY(),
                                     _navrect.getX(),
                                     _navrect.getHeight()));

      // RIGHT
      g2.fill(new Rectangle2D.Double(_navrect.getX() + _navrect.getWidth(),
                                     _navrect.getY(),
                                     w - _navrect.getX(),
                                     _navrect.getHeight()));

      // Draw the little nav box
      g2.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER));
      g2.setColor(_borderColor);
      g2.setStroke(_stroke);
      g2.draw(_navrect.getRect());

      _dirty = false;
    }// end if dirty

    g.drawImage(_buffer, 0, 0, this);
  }

  public void setDirty(boolean b) {
    _dirty = b;
  }

  public Dimension getMaximumSize() {
    return(getPreferredSize());
  }

  public Dimension getMinimumSize() {
    return(getPreferredSize());
  }

  public Dimension getPreferredSize() {
    return(new Dimension(_width, _height));
  }


  private void setLocation(Rectangle2D r, double x, double y) {
    r.setRect(x, y, r.getWidth(), r.getHeight());
  }

  private static void setLocation(Rectangle2D r, Point2D pt) {
    r.setRect(pt.getX(), pt.getY(), r.getWidth(), r.getHeight());
  }



  /*
   * Tell the target pane (the GraphScroller) to move the viewport.
   */
  private void updateTargetPane() {
    Point2D pt = new Point2D.Double(_navrect.getX(), _navrect.getY());
    Point2D wpt = mapViewportToComponent(pt);
    _dviewport.setViewportPosition(wpt);
  }




  private Point2D mapComponentToViewport(Point2D pt) {
    double sx = (_mapWidth * 1.0) / (_child.getBufferedComponent().getWidth() * 1.0);
    double sy = (_mapHeight * 1.0) / (_child.getBufferedComponent().getHeight() * 1.0);

    return(new Point2D.Double(pt.getX() * sx, pt.getY() * sy));
  }

  private Point2D mapViewportToComponent(Point2D pt) {
    double sx = (_mapWidth * 1.0) / (_child.getBufferedComponent().getWidth() * 1.0);
    double sy = (_mapHeight * 1.0) / (_child.getBufferedComponent().getHeight() * 1.0);

    return(new Point2D.Double(pt.getX() / sx, pt.getY() / sy));
  }



  /**
   * Manage the little orange navigation rectangle.
   */
  private class NavRect {

    /*
     * Keep the nav rect from moving off the right or bottom of the
     * panel.
     */
    private static final double __FUDGE = 1.0;

    private Rectangle2D __rect;

    public NavRect(double x, double y, double w, double h) {
      __rect = new Rectangle2D.Double(x, y, w, h);
    }

    public double getX() {
      return(__rect.getX());
    }

    public double getY() {
      return(__rect.getY());
    }

    public double getWidth() {
      return(__rect.getWidth());
    }

    public double getHeight() {
      return(__rect.getHeight());
    }

    public boolean contains(Point2D pt) {
      return(__rect.contains(pt));
    }

    public boolean moveTo(Point2D pt) {
      return(moveTo(pt.getX(), pt.getY()));
    }

    /*
     * This will not allow the rect to be moved outside of the boundry
     * of the panel.
     *
     * @return true if the rect has moved.
     */
    public boolean moveTo(double x, double y) {
      double oldx = __rect.getX();
      double oldy = __rect.getY();

      double pw = _width * 1.0;
      double ph = _height * 1.0;

      if (x < 0.0) {
        x = 0.0;
      }
      if (y < 0.0) {
        y = 0;
      }

      if ((x + getWidth()) > (pw - __FUDGE)) {
        x = pw - _navrect.getWidth() - __FUDGE;
      }
      if ((y + _navrect.getHeight()) > (ph - __FUDGE)) {
        y = ph - _navrect.getHeight() - __FUDGE;
      }

      if (x < 0.0) {
        x = 0.0;
      }
      if (y < 0.0) {
        y = 0.0;
      }

      if ((oldx == x) && (oldy == y)) {
        return(false);
      }
      else {
        __rect.setRect(x, y, __rect.getWidth(), __rect.getHeight());
        return(true);
      }
    }


    /*
     * Not all sizes accepted.  This will not allow a width or
     * height that is too large for the panel.
     */
    public void resize(double width, double height) {

      double pw = _width * 1.0;
      double ph = _height * 1.0;

      if ((__rect.getX() + width) > (pw - __FUDGE)) {
        width = pw - __rect.getX() - __FUDGE;
      }
      if ((__rect.getY() + height) > (ph - __FUDGE)) {
        height = ph - __rect.getY() - __FUDGE;
      }

      __rect.setRect(__rect.getX(), __rect.getY(), width, height);
    }


    public Rectangle2D getRect() {
      return(__rect);
    }


  }// end class "NavRect"


}
