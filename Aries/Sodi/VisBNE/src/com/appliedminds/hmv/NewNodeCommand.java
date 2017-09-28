package com.appliedminds.hmv;

import java.awt.*;
import java.awt.geom.*;
import java.util.Iterator;
import com.appliedminds.martini.*;
import com.appliedminds.martinix.fader.FaderUI2;
import com.appliedminds.martinix.fader.FaderUtil;
import com.appliedminds.martinix.ui.TextScreenData;

/**
 * HMWCommand implementation that knows how to draw a new node into a graph
 *
 * @author darin@apmindsf.com
 */
public class NewNodeCommand implements HMVCommand {

  private static final int DEFAULT_SLIDER_VALUE = 50;

  /** the controlling tool */
  private HMVSelectTool2 _hmvTool;

  /** where the new node should be placed */
  private Point _position;

  /**
   * Creates a new NewNodeCommand instance
   *
   * @param hmvTool the tool that's invoking this command
   * @param position where the new node should be placed; if null, the new
   * node will be placed in the center of the screen
   */
  public NewNodeCommand(HMVSelectTool2 hmvTool, Point position) {
    _hmvTool = hmvTool;
    _position = position;
  }


  public void setPosition(Point p) {
    _position = p;
  }


  /**
   * Draws a new node onto the graph
   */
  public void execute() {
    GraphPanel graphPanel = _hmvTool.getGraphPanel();
    DrawableNode newNode = graphPanel.getDrawableGraph().createNode();
    FaderUtil.setNodeLabel(newNode, "new node");
    FaderUtil.setSliderValue(newNode, DEFAULT_SLIDER_VALUE);
    FaderUtil.setSliderVisible(newNode, true);

    Size size = newNode.getPreferredSize(_hmvTool.getGraphics(),
                                         graphPanel.getScale());

    Point2D ew;
    if (_position == null) {
      // place the new node in the center
      Dimension panelSize = graphPanel.getSize();
      ew = graphPanel.mapViewportToWorld(new Point((int)panelSize.width/2,
                                                   (int)panelSize.height/2));
    }
    else {
      ew = graphPanel.mapViewportToWorld(_position);
    }

    Rectangle2D bounds =
      new Rectangle2D.Double(ew.getX() - (size.getWidth()/2),
                             ew.getY() + (size.getHeight()/2),
                             size.getWidth(),
                             size.getHeight());

    DrawableGraphContext ctx = graphPanel.getDrawableGraphContext();

    if (_position == null) {
      // avoid dropping the new node on top of any existing nodes
      while (true) {
        boolean foundMatch = false;
        for (NodeIterator i = graphPanel.getDrawableGraph().nodesIterator();
             i.hasNext(); ) {
          Rectangle2D nodeRect = ctx.getBounds(i.next());
          if (nodeRect != null && nodeRect.equals(bounds)) {
            foundMatch = true;
            break;
          }
        }
        if (foundMatch) {
          // adjust the position of the new node and try again
          bounds.setRect(bounds.getX() + 25, bounds.getY() - 25,
                         bounds.getWidth(), bounds.getHeight());
          continue;
        }
        else
          break;
      }
    }

    ctx.setBounds(newNode, bounds);
    ctx.setNeedsRepaint(newNode, true);

    // update immediately so we can get ready for edit state
    _hmvTool.clearSelectedNodes();
    _hmvTool.addSelectedNode(newNode);
    _hmvTool.setState(_hmvTool.STATE_SELECTED_NODE, new Object[] {newNode});
    Rectangle rect = graphPanel.getBounds();
    graphPanel.paintImmediately(rect.x, rect.y,
                                rect.width, rect.height);

    _hmvTool.fireGraphChanged();

    // find the context object associated with the label of the new node
    HitTester hit = null;
    for (Iterator itr=graphPanel.getHitTesters(newNode); itr.hasNext();)
    {
      hit = (HitTester) itr.next();
      if (hit.getDecorationId().equals(FaderUI2.DECORATION_NODE_TEXT)) {
        break;
      }
    }

    if (hit != null)
    {
      TextScreenData data = (TextScreenData) hit.getContext();
      Rectangle2D textRect = data.getBounds();
      _hmvTool.setState(_hmvTool.STATE_EDIT_NODE,
                        new Object[] {newNode, textRect});
      // getApp().editNodeLabel(newNode, textRect);
    }
    else {
      throw (new RuntimeException("HitTesterMap corrupt"));
    }
  }


  /**
   * Not yet implemented - does nothing
   */
  public void undo() {
  }


  /**
   * @return false - undo is not yet enabled
   */
  public boolean isUndoable() {
    return false;
  }


} // end class "NewNodeCommand"
