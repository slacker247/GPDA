package com.appliedminds.martini;

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.util.ArrayList;
import java.util.Iterator;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JLayeredPane;
import javax.swing.JPanel;
import javax.swing.JViewport;
import javax.swing.SwingUtilities;


/**
 * A JPanel that can send MARQUEE events to a MarqueeEventListener. This
 * is meant to be added to a JLayeredPane, preferrably in the PALLETE depth,
 * to avoid confusing the drop-down menus in an app.<p>
 *
 * The MarqueePane implments the ComponentListener interface, so it can
 * resize itself to the layer below. It should be added as a Listener to
 * the component whose size it should match. e.g.:<p>
 * <code>
 *    someComponent.addComponentListener(new MarqueePane());
 * </code>
 *
 * @author will@apmindsf.com
 * @author dae@apmindsf.com
 */
public class MarqueePane extends JPanel
  implements ComponentListener, MouseListener, MouseMotionListener
{
  private static final int THRESHOLD = 3;

  private ArrayList _marqueeListeners;
  private Point _mousePoint1;
  private Point _mousePoint2;
  private Rectangle _marqueeRect;
  private boolean _marqueeOn;
  private MarqueeContainer _marqueeContainer;
  private Component _geomSource;  // source of geometry events
  private Component _mouseSource; // source of mouse events
  private Point _topLeft;  // top left point in our own coordinate

  /**
   * @param marqueeContainer the frame to attach this marquee pane to
   */
  public MarqueePane(MarqueeContainer marqueeContainer)
  {
    super();
    setOpaque(false);
    setVisible(false); // default invisible

    _marqueeListeners = new ArrayList();
    _mousePoint1 = _mousePoint2 = null;
    _marqueeOn = false;

    _marqueeContainer = marqueeContainer;

    _geomSource = _mouseSource = null;
    _topLeft = null;

//    addMouseListener(this);
//    addMouseMotionListener(this);
  }

  /**
   * Throw the current rectangle onto the screen.
   */
  public void paintComponent(Graphics g) {
    if (_marqueeOn) {
      g.drawRect(_marqueeRect.x, _marqueeRect.y,
                 _marqueeRect.width, _marqueeRect.height);
    }
  }


  /**
   * @param ml where we will send marquee events to.
   */
  public void addMarqueeListener(MarqueeListener ml) {
    _marqueeListeners.add(ml);
  }


  /**
   * Tells the MarqueePane to respond to events coming from the
   * given component. Conceptually this is the component we will
   * marquee on.
   *
   * @param comp the component whose events we will listen to.
   */
  public void setEventSource(Component comp) {
    comp.addMouseListener(this);
    comp.addMouseMotionListener(this);
    comp.addComponentListener(this);
  }


  //////////////////////////////////////////////////
  // ComponentListener API
  public void componentHidden(ComponentEvent e) {}
  public void componentMoved(ComponentEvent e) {}
  public void componentShown(ComponentEvent e) {}

  public void componentResized(ComponentEvent e) {
    if (_geomSource == null)
      _geomSource = e.getComponent();

    this.setSize(_geomSource.getSize());
  }
  // EndComponentListener API
  //////////////////////////////////////////////////


  //////////////////////////////////////////////////
  // MouseListener API

  public void mouseClicked(MouseEvent e) {}
  public void mouseEntered(MouseEvent e) {}
  public void mouseExited(MouseEvent e) {}

  public void mousePressed(MouseEvent e)
  {
    if (_mouseSource == null) {
      _mouseSource = e.getComponent();
      _topLeft = SwingUtilities.convertPoint(_mouseSource,
                                             new Point(0,0),
                                             this);
    }

    if (!SwingUtilities.isLeftMouseButton(e)) {
      return;
    }

    _mousePoint1 = SwingUtilities.convertPoint(_mouseSource,
                                               e.getPoint(), this);
  }

  public void mouseReleased(MouseEvent e)
  {
    //    System.err.println("marquee pane released");
    if (!SwingUtilities.isLeftMouseButton(e)) {
      return;
    }

    // clean up
    if (_mousePoint2 != null) {
      _marqueeRect = extractRect(_mousePoint1, _mousePoint2);
      repaint(_marqueeRect.x, _marqueeRect.y,
              _marqueeRect.width+1, _marqueeRect.height+1);
    }

    _mousePoint2 = SwingUtilities.convertPoint(_mouseSource,
                                               e.getPoint(),
                                               this);

    if ((Math.abs(_mousePoint1.x - _mousePoint2.x) > THRESHOLD) &&
        (Math.abs(_mousePoint1.y - _mousePoint2.y) > THRESHOLD))
      fireMarqueeEvent(_mousePoint1, _mousePoint2);

    _marqueeOn = false;
    _mousePoint1 = _mousePoint2 = null;
  }

  // EndMouseListener API
  //////////////////////////////////////////////////


  //////////////////////////////////////////////////
  // MouseMotionListener API

  public void mouseDragged(MouseEvent e) {

    if (!SwingUtilities.isLeftMouseButton(e)) {
      return;
    }

    _marqueeOn = true;

    if (_mousePoint2 != null) {
      _marqueeRect = extractRect(_mousePoint1, _mousePoint2);
      repaint(_marqueeRect.x, _marqueeRect.y,
              _marqueeRect.width+1, _marqueeRect.height+1);
    }

    _mousePoint2 = SwingUtilities.convertPoint(_mouseSource,
                                               e.getPoint(),
                                               this);

    _marqueeRect = extractRect(_mousePoint1, _mousePoint2);
    repaint(_marqueeRect.x, _marqueeRect.y,
            _marqueeRect.width+1, _marqueeRect.height+1);
  }

  public void mouseMoved(MouseEvent e) {}

  // MouseMotionListener API
  //////////////////////////////////////////////////



  /**
   * Create a Rectangle from two arbitrary points.
   *
   * @param pt1 mouse press point
   * @param pt2 mouse drag point
   * @return a rectangle bounded by these two points.
   */
  private Rectangle extractRect(Point pt1, Point pt2)
  {
    int x, y, w, h;

    // make sure the mouse drag point is within our bounds. we use the
    // smaller geometries of either _mouseSource or _geomSource (same as this)
    Point pt3 = new Point(pt2);

    int minX = _topLeft.x;
    int maxX1 = this.getX() + _mouseSource.getWidth();
    int maxX2 = _topLeft.x + this.getWidth();
    int maxX = (maxX1 > maxX2) ? maxX2 : maxX1;

    int minY = _topLeft.y;
    int maxY1 = this.getY() + _mouseSource.getHeight();
    int maxY2 = _topLeft.y + this.getHeight();
    int maxY = (maxY1 > maxY2) ? maxY2 : maxY1;

    Point bottomRight =
      SwingUtilities.convertPoint(_geomSource,
                                  new Point(maxX, maxY),
                                  _marqueeContainer.getMarqueeLayer());
    maxX = bottomRight.x;
    maxY = bottomRight.y;

    // limit the marquee at the boundaries
    if (pt3.x < minX) {
      pt3.x = minX;
    }
    else if (pt3.x > maxX) {
      pt3.x = maxX;
    }

    if (pt3.y < minY) {
      pt3.y = minY;
    }
    else if (pt3.y > maxY) {
      pt3.y = maxY;
    }

    // get the top left corner of the rectangle from pt1 and pt3
    if (pt3.x >= pt1.x) {
      x = pt1.x;
    }
    else {
      x = pt3.x;
    }

    w = Math.abs(pt3.x - pt1.x);

    if (pt3.y >= pt1.y) {
      y = pt1.y;
    }
    else {
      y = pt3.y;
    }

    h = Math.abs(pt3.y - pt1.y);

    return (new Rectangle(x, y, w-1, h-1));
  }



  /**
   * Dispatch the MarqueeSelection event to any subscribed listener. The
   * rectangle will be in the same coordinate system as the mouse event
   * originator's.
   *
   * @param pt1 point 1
   * @param pt2 point 2
   */
  private void fireMarqueeEvent(Point pt1, Point pt2) {
    Rectangle r = extractRect(pt1, pt2);
    Point tl = new Point(r.x, r.y);
    Point newtl = SwingUtilities.convertPoint (this,
                                               new Point(r.x, r.y),
                                               _mouseSource);
    r = new Rectangle(newtl, r.getSize());
    //    System.err.println("marquee rect: " + r);
    for(Iterator it = _marqueeListeners.iterator(); it.hasNext();) {
      MarqueeListener listener = (MarqueeListener) it.next();
      listener.marqueeSelected(r);
    }
  }

} // end class MarqueePane
