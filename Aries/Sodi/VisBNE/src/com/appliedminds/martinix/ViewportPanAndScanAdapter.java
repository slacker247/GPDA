package com.appliedminds.martinix;


import com.appliedminds.martini.GraphPanel;
import com.appliedminds.martini.BufferedComponent;
import com.appliedminds.martini.DynamicViewport;
import com.appliedminds.martini.PanAndScanPanel;
import javax.swing.JViewport;
import java.awt.Point;
import java.awt.geom.Point2D;
import java.awt.Dimension;
import javax.swing.event.ChangeListener;
import javax.swing.event.ChangeEvent;



/**
 * Adapter code to link a JViewport with a PanAndScanPanel.
 *
 * <p>Once properly configured, this class will make sure that the
 * viewport reflects the state of the pan and scan panel and
 * vice-versa.
 *
 *
 * @author mathias@apmindsf.com
 */
public class ViewportPanAndScanAdapter
  implements DynamicViewport
{
  private BufferedComponent _targpanel;
  private Point _lastViewportPosition;
  private Dimension _lastViewportExtent;
  private Dimension _lastPanelSize;
  private JViewport _viewport;
  private PanAndScanPanel _panner;


  /**
   * Create a new adapter.
   *
   * @param targ the component that is being displayed by the viewport.
   * @param viewport the viewport containing the targ component.
   * @param mini the pan and scan panel.
   */
  public ViewportPanAndScanAdapter(BufferedComponent targ,
                                    JViewport viewport,
                                    PanAndScanPanel mini)
  {
    _viewport = viewport;
    _targpanel = targ;
    _lastViewportPosition = new Point(0,0);
    _lastViewportExtent = new Dimension(0,0);
    _lastPanelSize = new Dimension(0,0);
    _panner = mini;
    _panner.setDynamicViewport(this);

    _viewport.addChangeListener
      (new ChangeListener() {
          public void stateChanged(ChangeEvent e) {
            JViewport port = (JViewport) e.getSource();

            // See if the viewport has moved in respect to the world:
            Point p = port.getViewPosition();
            if (!p.equals(_lastViewportPosition)) {
              _lastViewportPosition.setLocation(p);
              _panner.updateViewportPosition(p);
            }

            // See if the size of the viewport has changed.
            updateViewportSize(port);
          }
        });
  }


  private void updateViewportSize(JViewport port) {
    Dimension dim = port.getExtentSize();
    Dimension pdim = _targpanel.getBufferedComponent().getSize();

    if (!(dim.equals(_lastViewportExtent) && pdim.equals(_lastPanelSize))) {

      double wfactor =
        dim.getWidth() / pdim.getWidth();

      double hfactor =
        dim.getHeight() / pdim.getHeight();

      _lastViewportExtent.setSize(dim);
      _lastPanelSize.setSize(pdim);
      _panner.updateNavBoxFactors(wfactor, hfactor);
    }
  }


  /**
   * Part of the DynamicViewport interface, updates the viewport
   * position.
   */
  public void setViewportPosition(Point2D wpt)
  {
    //
    // Do not allow the client to ask the scroll bars to scroll more
    // than is possible (in the positive direction).
    //

    double x = wpt.getX();
    double y = wpt.getY();

    Dimension ext = _viewport.getExtentSize();

    int width = _targpanel.getBufferedComponent().getWidth();
    int height = _targpanel.getBufferedComponent().getHeight();

    if ((x + ext.getWidth()) > width) {
      x = width - ext.getWidth();
    }
    if ((y + ext.getHeight()) > height) {
      y = height - ext.getHeight();
    }

    Point newpos = new Point((int) x,  (int) y);
    _viewport.setViewPosition(newpos);
  }

}// end class "GraphPanelScrollerAdapter"

