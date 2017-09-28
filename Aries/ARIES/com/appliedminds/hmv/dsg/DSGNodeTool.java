package com.appliedminds.hmv.dsg;

import com.appliedminds.hmv.*;
import com.appliedminds.martini.*;
import com.appliedminds.martinix.fader.FaderUtil;

import java.awt.Cursor;
import java.awt.Graphics2D;
import java.awt.event.MouseEvent;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.util.Iterator;
import javax.swing.SwingUtilities;


/**
 * The node tool for the DSGViewer.
 */
public class DSGNodeTool extends DSGTool {

  private static final boolean EDIT_ON_ADD = false;

  private DSGCommonTool _commonTool;

  public DSGNodeTool(Cursor c, DSGViewer app, DSGCommonTool commonTool) {
    super(c, app);
    _commonTool = commonTool;
  }


  /**
   * Handle mouse event on node.
   */
  public boolean handleNodeMouseEvent(DrawableGraphMouseEvent e) {
    MouseEvent me = e.getMouseEvent();

    if (mousePressed(e) || me.isPopupTrigger()) {
      if (SwingUtilities.isLeftMouseButton(me) &&
          !_commonTool.isNodePopupMenuVisible())
      {
        _commonTool.showNodeProperties((DrawableNode)e.getGraphElement());
      }
      else if (me.isPopupTrigger()) {
        _commonTool.showNodePopupMenu((DrawableNode)e.getGraphElement(), me);
      }
    }
    return (true);
  }


  /**
   * Handle mouse event on node text.
   */
  public boolean handleNodeTextMouseEvent(DrawableGraphMouseEvent e) {
    return (handleNodeMouseEvent(e));
  }


  /**
   * Handle mouse event on hit missed (click on white space).
   */
  public boolean handleHitTestMissedMouseEvent(DrawableGraphMouseEvent dme) {
    MouseEvent e = dme.getMouseEvent();

    if (mousePressed(dme) &&
        SwingUtilities.isLeftMouseButton(e)) { // left button
      DrawableNode newNode =
        this.getGraphPanel().getDrawableGraph().createNode();
      FaderUtil.setNodeLabel(newNode, "new node");

      Size size = newNode.getPreferredSize(this.getGraphics(), this.getGraphPanel().getScale());

      Point2D ew = this.getGraphPanel().mapViewportToWorld(e.getPoint());

      Rectangle2D bounds =
        new Rectangle2D.Double(ew.getX() - (size.getWidth()/2),
                               ew.getY() + (size.getHeight()/2),
                               size.getWidth(),
                               size.getHeight());

      DrawableGraphContext ctx =
        this.getGraphPanel().getDrawableGraphContext();
      ctx.setBounds(newNode, bounds);
      ctx.setNeedsRepaint(newNode, true);

      this.getGraphPanel().repaint();

      if (EDIT_ON_ADD) {
        // popup a node properties dialog
        final GraphElementPropertiesDialog dialog =
          new GraphElementPropertiesDialog(getApp());
        DSGNodeProperties nodeProps = new DSGNodeProperties(newNode);
        dialog.setGraphElementProperties(nodeProps);

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

    return (true);
  }

} // end class "DSGNodeTool"

