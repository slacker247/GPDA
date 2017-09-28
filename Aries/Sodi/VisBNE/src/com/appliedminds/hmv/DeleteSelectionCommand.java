package com.appliedminds.hmv;

import java.util.*;
import com.appliedminds.martini.*;

/**
 * HMWCommand implementation that knows how to delete the currently selected
 * graph elements
 *
 * @author darin@apmindsf.com
 */
public class DeleteSelectionCommand implements HMVCommand {

  /** the nodes that will be deleted */
  private ArrayList _nodesToDelete;

  /** the edges that will be deleted */
  private ArrayList _edgesToDelete;

  /** the tool controlling this command */
  private HMVSelectTool2 _tool;


  /**
   * Creates a new DeleteSelectionCommand instance
   *
   * @param tool the controlling tool
   * @param elements the list of nodes and edges to be deleted
   */
  public DeleteSelectionCommand(HMVSelectTool2 tool, Iterator elements) {
    _tool = tool;
    _nodesToDelete = new ArrayList();
    _edgesToDelete = new ArrayList();

    while (elements.hasNext()) {
      Object o = elements.next();
      if (o instanceof DrawableNode) 
        _nodesToDelete.add(o);
      else if (o instanceof DrawableEdge) 
        _edgesToDelete.add(o);
    }
  }


  /**
   * Creates a new DeleteSelectionCommand instance
   *
   * @param tool the controlling tool
   * @param nodes the list of nodes to be deleted (may be null)
   * @param edges the list of edges to be deleted (may be null)
   */
  public DeleteSelectionCommand(HMVSelectTool2 tool, Iterator nodes, 
                                Iterator edges) {
    _tool = tool;
    _nodesToDelete = iteratorToArrayList(nodes);
    _edgesToDelete = iteratorToArrayList(edges);
  }


  /**
   * Sets the nodes that will be deleted, replacing the node list currently
   * being managed by this command
   *
   * @param nodes the nodes to be deleted
   */
  public void setNodesToDelete(Iterator nodes) {
    _nodesToDelete = iteratorToArrayList(nodes);
  }


  /**
   * Sets the edges that will be deleted, replacing the edge list currently
   * being managed by this command
   *
   * @param nodes the edges to be deleted
   */
  public void setEdgesToDelete(Iterator edges) {
    _edgesToDelete = iteratorToArrayList(edges);
  }


  /**
   * Deletes the selected nodes and edges
   */
  public void execute() {
    if (_nodesToDelete.size() == 0 && _edgesToDelete.size() == 0) {
      return;
    }
    DrawableGraph graph = _tool.getGraphPanel().getDrawableGraph();

    // first "erase" it from the visual first
    Iterator itr = _nodesToDelete.iterator();
    while (itr.hasNext()) {
      DrawableNode node = (DrawableNode)itr.next();
      if (node == null)
        continue; // impossible?

      // remove the edges connected to this node as well
      EdgeIterator edges = node.getIncomingEdges();
      while (edges.hasNext()) {
        DrawableEdge edge = edges.next();
        _edgesToDelete.add(edge);
      }
      edges = node.getOutgoingEdges();
      while (edges.hasNext()) {
        DrawableEdge edge = edges.next();
        _edgesToDelete.add(edge);
      }

      // remove node from graph
      graph.remove(node);
    }

    itr = _edgesToDelete.iterator();
    while (itr.hasNext()) {
      DrawableEdge edge = (DrawableEdge)itr.next();

      // remove edge from graph
      graph.remove(edge);
    }

    _tool.getGraphPanel().redraw();

    // recalculate probabilites
    //initCalculationPath2(graph, null, _cpath, true);
    //recalculateProbabilityUsingPath2(null, _cpath, getGraphPanel());

    _tool.getGraphPanel().paintImmediately();
    _tool.fireGraphChanged();

    _tool.setState(_tool.STATE_IDLE, null);
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


  /**
   * Converts the given Iterator to a new ArrayList - if the Iterator is null
   * or empty, this method returns an empty ArrayList (never null)
   *
   * @param itr the Iterator to parse
   * @return a new ArrayList made up of elements from the Iterator
   */
  public ArrayList iteratorToArrayList(Iterator itr) {
    ArrayList newList = new ArrayList();
    if (itr != null) {
      while (itr.hasNext())
        newList.add(itr.next());
    }
    return newList;
  }


} // end class "DeleteSelectionCommand"
