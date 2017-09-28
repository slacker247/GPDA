package com.appliedminds.hmv.dsg;

import com.appliedminds.martini.DrawableNode;
import com.appliedminds.martini.DrawableEdge;
import java.awt.event.MouseEvent;

/**
 * The common tool interface for the DSGViewer
 *
 * @author daepark@apmindsf.com
 */
public interface DSGCommonTool {

  /**
   * Show the node properties.
   */
  public void showNodeProperties(DrawableNode node);


  /**
   * Show the node popup menu.
   */
  public void showNodePopupMenu(DrawableNode node, MouseEvent e);


  /**
   * Is the node popup menu visible?
   */
  public boolean isNodePopupMenuVisible();


  /**
   * Show the edge properties.
   */
  public void showEdgeProperties(DrawableEdge edge);


  /**
   * Show the edge popup menu.
   */
  public void showEdgePopupMenu(DrawableEdge edge, MouseEvent e);


  /**
   * Is the edge popup menu visible?
   */
  public boolean isEdgePopupMenuVisible();

} // end interface "DSGCommonTool"
