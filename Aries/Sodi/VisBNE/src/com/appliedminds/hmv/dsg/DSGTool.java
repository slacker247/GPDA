package com.appliedminds.hmv.dsg;

import java.awt.Graphics2D;
import java.awt.geom.Rectangle2D;
import java.awt.event.MouseEvent;
import java.awt.Cursor;
import com.appliedminds.martini.*;
import com.appliedminds.martinix.gapp.*;


/**
 * An extension of GTool to specfically accomodate the
 * DSGViewer.
 */
public abstract class DSGTool extends GToolAdapter {

  protected DSGViewer _app;

  public DSGTool(Cursor c, DSGViewer app) {
    super(c);
    _app = app;
  }

  /**
   * Default impl returns false, override if you want this event.
   */
  public boolean handleEdgeIconMouseEvent(DrawableGraphMouseEvent e) {
    return(false);
  }

  /**
   * Default impl returns false, override if you want this event.
   */
  public boolean handleNodeMouseEvent(DrawableGraphMouseEvent e) {
    return (false);
  }

  /**
   * Default impl returns false, override if you want this event.
   */
  public boolean handleNodeTextMouseEvent(DrawableGraphMouseEvent e) {
    return (false);
  }

  /**
   * Default impl returns false, override if you want this event.
   */
  public boolean handleEdgeMouseEvent(DrawableGraphMouseEvent e) {
    return (false);
  }

  /**
   * Default impl returns false, override if you want this event.
   */
  public boolean handleHitTestMissedMouseEvent(DrawableGraphMouseEvent e) {
    return (false);
  }

  /**
   * Default impl returns false, override if you want this event.
   */
  public void handleMultipleHitTestSuccess(DrawableGraphMultipleSelectionEvent evt) { }


  /**
   * Default impl returns false, override if you want this event.
   */
  public void handleMultipleHitTestMissedGraph(Rectangle2D rectangle) { }


  protected GraphPanel getGraphPanel() {
    return(_app.getGraphPanel());
  }

  protected DSGViewer getApp() {
    return (_app);
  }

  protected  Graphics2D getGraphics() {
    return ((Graphics2D) _app.getGraphPanel().getGraphics());
  }

  /**
   * @return true if the mouse was pressed.
   */
  protected boolean mousePressed(DrawableGraphMouseEvent e) {
    return(e.getMouseEvent().getID() == MouseEvent.MOUSE_PRESSED);
  }

  /**
   * @return true if the mouse was released.
   */
  protected final boolean mouseReleased(DrawableGraphMouseEvent e) {
    return(e.getMouseEvent().getID() == MouseEvent.MOUSE_RELEASED);
  }

  /**
   * @return true if the mouse was clicked
   */
  protected final boolean mouseClicked(DrawableGraphMouseEvent e) {
    return(e.getMouseEvent().getID() == MouseEvent.MOUSE_CLICKED);
  }

} // end class "DSGTool"


