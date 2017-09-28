package com.appliedminds.hmv;

import java.util.*;

import com.appliedminds.martini.*;

/**
 * Interface for classes that know how to maintain lists of selected nodes and
 * edges
 *
 * @author darin@apmindsf.com
 */
public abstract class HMVSelectionManager {

  /** the list of selected nodes */
  private HashSet _selectedNodes = new HashSet();

  /** the list of selected edges */
  private HashSet _selectedEdges = new HashSet();


  /**
   * Sets the node and/or display properties needed to mark a node as selected -
   * this must be implemented by subclasses
   *
   * @param node the node to be updated
   * @param isSelected if true, the node should be marked as selected; otherwise
   * it should be marked as unselected
   */
  protected abstract void updateSelectionProperties(DrawableNode node,
                                                    boolean isSelected);


  /**
   * Sets the edge and/or display properties needed to mark a edge as selected -
   * this must be implemented by subclasses
   *
   * @param edge the edge to be updated
   * @param isSelected if true, the edge should be marked as selected; otherwise
   * it should be marked as unselected
   */
  protected abstract void updateSelectionProperties(DrawableEdge edge,
                                                    boolean isSelected);


  /**
   * Determines if a node is currently selected
   *
   * @param node the node to be checked
   * @return true if the node is current selected
   */
  public boolean isSelected(DrawableNode node) {
    return _selectedNodes.contains(node);
  }


  /**
   * Determines if a edge is currently selected
   *
   * @param edge the edge to be checked
   * @return true if the edge is current selected
   */
  public boolean isSelected(DrawableEdge edge) {
    return _selectedEdges.contains(edge);
  }


  /**
   * Marks the given node as selected or unselected (depending on the value of
   * isSelected)
   *
   * @param node the node to be modified
   * @param isSelected if true, the node will be marked selected; if false, it
   * will be marked unselected
   */
  public void setSelected(DrawableNode node, boolean isSelected) {
    if (isSelected)
      _selectedNodes.add(node);
    else
      _selectedNodes.remove(node);
    updateSelectionProperties(node, isSelected);
  }


  /**
   * Marks the given edge as selected or unselected (depending on the value of
   * isSelected)
   *
   * @param edge the edge to be modified
   * @param isSelected if true, the edge will be marked selected; if false, it
   * will be marked unselected
   */
  public void setSelected(DrawableEdge edge, boolean isSelected) {
    if (isSelected)
      _selectedEdges.add(edge);
    else
      _selectedEdges.remove(edge);
    updateSelectionProperties(edge, isSelected);
  }


  /**
   * @return the number of currently selected nodes
   */
  public int selectedNodesCount() {
    return _selectedNodes.size();
  }


  /**
   * @return the number of currently selected edges
   */
  public int selectedEdgesCount() {
    return _selectedEdges.size();
  }


  /**
   * @return a Iterator containing the currently selected nodes
   */
  public Iterator getSelectedNodes() {
    return _selectedNodes.iterator();
  }


  /**
   * @return a Iterator containing the currently selected edges
   */
  public Iterator getSelectedEdges() {
    return _selectedEdges.iterator();
  }


  /**
   * @return a Iterator containing all selected elements (i.e nodes and edges)
   */
  public Iterator getSelectedElements() {
    HashSet allElements = new HashSet();

    for (Iterator i = _selectedNodes.iterator(); i.hasNext(); )
      allElements.add(i.next());
    for (Iterator i = _selectedEdges.iterator(); i.hasNext(); )
      allElements.add(i.next());

    return allElements.iterator();
  }


  /**
   * Toggles the selected status of the given node, i.e. if it's currently
   * selected, it will be unselected, and vice versa
   *
   * @param node the node to be toggled
   */
  public void toggleSelection(DrawableNode node) {
    setSelected(node, !isSelected(node));
  }


  /**
   * Toggles the selected status of the given edge, i.e. if it's currently
   * selected, it will be unselected, and vice versa
   *
   * @param edge the edge to be toggled
   */
  public void toggleSelection(DrawableEdge edge) {
    setSelected(edge, !isSelected(edge));
  }


  /**
   * Clears the list of selected nodes, marking each currently selected node as
   * "not selected"
   */
  public void clearSelectedNodes() {
    for (Iterator i = _selectedNodes.iterator(); i.hasNext(); )
      updateSelectionProperties((DrawableNode)i.next(), false);
    _selectedNodes.clear();
  }


  /**
   * Clears the list of selected edges, marking each currently selected edge as
   * "not selected"
   */
  public void clearSelectedEdges() {
    for (Iterator i = _selectedEdges.iterator(); i.hasNext(); )
      updateSelectionProperties((DrawableEdge)i.next(), false);
    _selectedEdges.clear();
  }


  /**
   * Clears the lists of selected nodes and edges, marking all currently
   * selected nodes and edges as "not selected"
   */
  public void clearAllSelections() {
    clearSelectedNodes();
    clearSelectedEdges();
  }

} // end class "HMVSelectionManager"
