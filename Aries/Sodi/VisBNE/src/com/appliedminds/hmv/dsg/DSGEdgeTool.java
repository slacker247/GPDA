package com.appliedminds.hmv.dsg;

import com.appliedminds.hmv.*;
import com.appliedminds.martini.*;
import com.appliedminds.martinix.ui.BorderEdgeAttachStrategy;
import com.appliedminds.martinix.fader.FaderUtil;
import java.awt.Color;
import java.awt.Cursor;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.event.MouseEvent;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.util.Iterator;
import java.util.logging.Logger;
import java.util.List;
import java.util.ArrayList;
import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;


/**
 * The edge tool for dsg.
 *
 * @author will@apmindsf.com
 */
public class DSGEdgeTool extends DSGTool {

  private static final boolean EDIT_ON_ADD = false;

  private static final String SUPPORTS             = "supports";
  private static final String REFUTES              = "refutes";

  private EdgeToolState STATE_IDLE = new StateIdle();
  private EdgeToolState STATE_DRAG = new StateDrag();

  private EdgeToolState _state;          // current state
  private DrawableNode _fromNode;        // the tail node
  private Point _fromPt;
  private Point _toPt;

  private BorderEdgeAttachStrategy _attachStrategy;

  private DSGCommonTool _commonTool;

  public DSGEdgeTool(Cursor cursor, DSGViewer app, DSGCommonTool commonTool) {
    super(cursor, app);
    _fromPt = _toPt = null;
    _state = STATE_IDLE;
    _attachStrategy = app.getFaderUIPrefs().getBorderEdgeAttachStrategy();
    _commonTool = commonTool;
  }


  /**
   * Handle mouse event on node.
   */
  public boolean handleEdgeMouseEvent(DrawableGraphMouseEvent e) {
    MouseEvent me = e.getMouseEvent();

    if (mousePressed(e) || me.isPopupTrigger()) {
      if (SwingUtilities.isLeftMouseButton(me) &&
          !_commonTool.isEdgePopupMenuVisible())
      {
        _commonTool.showEdgeProperties((DrawableEdge)e.getGraphElement());
      }
      else if (me.isPopupTrigger()) {
        _commonTool.showEdgePopupMenu((DrawableEdge)e.getGraphElement(), me);
      }
    }
    return (true);
  }


  public boolean handleNodeMouseEvent(DrawableGraphMouseEvent e) {
    return (handleNodeEvent(e));
  }

  public boolean handleNodeTextMouseEvent(DrawableGraphMouseEvent e) {
    return (handleNodeEvent(e));
  }

  private boolean handleNodeEvent(DrawableGraphMouseEvent e)
  {
    if (mousePressed(e)) {
      _state.mousePressedNode((DrawableNode) e.getGraphElement(),
                              e.getMouseEvent());
    }
    else if (mouseReleased(e)) {
      _state.mouseReleasedNode((DrawableNode) e.getGraphElement(),
                               e.getMouseEvent());
    }

    return (true);
  }

  public void mouseDragged(MouseEvent e) {
    if (getGraphPanel().getDrawableGraph() != null) {
      _state.mouseDragged(e);
    }
  }

  public void mouseReleased(MouseEvent e) {
    if (getGraphPanel().getDrawableGraph() != null) {
      _state.mouseReleased(e);
    }
  }

  private void rubberBand(Point from, Point to) {
    Graphics2D g = getGraphics();
    //    g.setColor(Color.blue);
    g.setColor(new Color(0xCE, 0xD7, 0xEF));
    g.setXORMode(getGraphPanel().getBackground());
    g.drawLine(from.x, from.y, to.x, to.y);
  }


  class EdgeToolState
  {
    void mousePressedNode(DrawableNode node, MouseEvent e) {}
    void mouseDragged(MouseEvent e) {}
    void mouseReleasedNode(DrawableNode node, MouseEvent e) {}
    void mouseReleased(MouseEvent e) {}
  }

  class StateIdle extends EdgeToolState {

    // save the fromNode and go into dragging state
    public void mousePressedNode(DrawableNode node, MouseEvent e) {
      _fromNode = node;
      _state = STATE_DRAG;

      DrawableGraphContext ctx = getGraphPanel().getDrawableGraphContext();

      // use the center of the node since we don't have a second point yet
      Rectangle2D bounds =
        getGraphPanel().mapWorldToViewport(ctx.getBounds(node));
      _fromPt = new Point((int)bounds.getCenterX(), (int)bounds.getCenterY());
    }

  } // end class StateIdle



  class StateDrag extends EdgeToolState {

    // update the rubberband
    public void mouseDragged(MouseEvent e)
    {
      if ((_fromPt != null) && (_toPt != null)) {
        // erase the last marquee line
        rubberBand(_fromPt, _toPt);
      }

      if (_fromNode != null) {
        _toPt = new Point(e.getX(), e.getY());

        // find the attachPoint for the from point
        DrawableGraphContext ctx = getGraphPanel().getDrawableGraphContext();

        Point2D toPtWorld =
          getGraphPanel().mapViewportToWorld(new Point2D.Double(_toPt.x,
                                                                _toPt.y));
        Rectangle2D bounds =
          (Rectangle2D) ctx.getContextData(_fromNode, "bounds");

        Point2D newFromPt =
          _attachStrategy.findAttachPoint(bounds,
                                          toPtWorld,
                                          _attachStrategy.RELATIVE);
        Point2D fromPtVP = getGraphPanel().mapWorldToViewport(newFromPt);
        _fromPt = new Point((int)fromPtVP.getX(), (int)fromPtVP.getY());
        rubberBand(_fromPt, _toPt);
      }
    }


    // erase the edge if we didn't land on a node
    public void mouseReleased(MouseEvent e) {
      if ((_fromPt != null) && (_toPt != null)) {
        rubberBand(_fromPt, _toPt);
        _fromPt = _toPt = null;
        _fromNode = null;
      }
      _state = STATE_IDLE;
    }


    // create an edge between _fromNode and node
    public void mouseReleasedNode(DrawableNode node, MouseEvent e)
    {
      if ((_fromNode != null) && !_fromNode.equals(node))
      {
        if (findEdge(node, _fromNode) != null)
        {
          // no multiple edges allowed
          //
          Logger.getLogger(_app.LOG_NAME).warning("WARNING: DSG does not allow multiple edges");
        }
        else {
          DrawableEdge newEdge = createEdge(node, _fromNode);

          // set up edge properties
          FaderUtil.setEdgeLabel(newEdge, "new relationship");

          DrawableGraphContext ctx =
            getGraphPanel().getDrawableGraphContext();
          ctx.setNeedsRepaint(_fromNode, true);
          ctx.setNeedsRepaint(newEdge, true);
          ctx.setNeedsRepaint(node, true);
          getGraphPanel().repaint();
          _fromNode = null;

          if (EDIT_ON_ADD) {
            // popup a edge properties dialog
            final GraphElementPropertiesDialog dialog =
              new GraphElementPropertiesDialog(getApp());
            DSGEdgeProperties edgeProps = new DSGEdgeProperties(newEdge);
            dialog.setGraphElementProperties(edgeProps);

            final DialogHandlerAdapter handler = new DialogHandlerAdapter() {
                public void clickedOk() {
                  if (dialog.propertiesChanged()) {
                    getGraphPanel().repaint();

                  }
                  dialog.setVisible(false);
                }
                public void clickedApply() {
                  if (dialog.propertiesChanged()) {
                    getGraphPanel().repaint();
                  }
                }
                public void clickedCancel() {
                  if (dialog.propertiesChanged()) {
                    getGraphPanel().repaint();

                  }
                  dialog.setVisible(false);
                }
              };
            dialog.addDialogHandler(handler);
            dialog.setVisible(true);
          }
        }
      }

      if ((_fromPt != null) && (_toPt != null)) {
        rubberBand(_fromPt, _toPt);
        _fromPt = _toPt = null;
      }

      _state = STATE_IDLE;
    }


    /**
     * Create an edge in the exising graph. Set TMV specific properties
     * and handle incorporating the new edge into any existing edge.
     *
     * @param the target node (head node)
     * @param the source node (tail node)
     */
    private DrawableEdge createEdge(DrawableNode toNode,
                                    DrawableNode fromNode)
    {
      DrawableGraph graph = getGraphPanel().getDrawableGraph();
      return (graph.createEdge(toNode, fromNode));
    }


    /**
     * Find an edge between the two given nodes, if any.
     *
     * @param head
     * @param tail
     * @return an edge from tail to head, or null if none.
     */
    private DrawableEdge findEdge(DrawableNode head, DrawableNode tail)
    {
      DrawableGraph graph = getGraphPanel().getDrawableGraph();

      // using edges for now, but might be more efficient to iterate
      // through nodes, (depending on the graph).
      for (EdgeIterator it=graph.edgesIterator(); it.hasNext();) {
        DrawableEdge edge = it.next();
        DrawableNode h = edge.getHead();
        DrawableNode t = edge.getTail();

        if (head == h && tail == t) {
          return (edge);
        }
      }

      return (null);
    }

  } // end class StateDrag

} // end class DSGEdgeTool
