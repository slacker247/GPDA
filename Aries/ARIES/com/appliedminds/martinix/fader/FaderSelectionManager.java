package com.appliedminds.martinix.fader;

import java.util.*;

import com.appliedminds.hmv.HMVSelectionManager;
import com.appliedminds.martini.*;

/**
 * HMVSelectionManager for the FaderUI
 *
 * @author darin@apmindsf.com
 */
public class FaderSelectionManager extends HMVSelectionManager {

  /**
   * Sets the node and/or display properties needed to mark a node as selected
   *
   * @param node the node to be updated
   * @param isSelected if true, the node should be marked as selected; otherwise
   * it should be marked as unselected
   */
  protected void updateSelectionProperties(DrawableNode node, 
                                           boolean isSelected) {
    FaderUtil.setSelected(node, isSelected);
    FaderUtil.setConnectorHubsVisible(node, isSelected);
    FaderUtil.setSliderToggleVisible(node, isSelected);
  }


  /**
   * Sets the edge and/or display properties needed to mark a edge as selected
   *
   * @param edge the edge to be updated
   * @param isSelected if true, the edge should be marked as selected; otherwise
   * it should be marked as unselected
   */
  protected void updateSelectionProperties(DrawableEdge edge,
                                           boolean isSelected) {
    FaderUtil.setSelected(edge, isSelected);
  }


} // end class "FaderSelectionManager"
