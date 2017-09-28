package com.appliedminds.hmv;

import com.appliedminds.martini.*;
import com.appliedminds.martinix.fader.*;

import java.awt.event.ActionEvent;
import javax.swing.AbstractAction;


/**
 * Action object that knows how to delet a support icon from the graph
 * panel.
 *
 * @author daepark@apmindsf.com
 */
public class SupportIconDeleteAction extends AbstractAction {

  private DrawableGraphElement _currentElt = null;
  private FaderIconHitContext _currentContext = null;
  private GraphPanel _graphPanel = null;


  public SupportIconDeleteAction(GraphPanel graphPanel) {
    super("Delete Supporting Document");
    _graphPanel = graphPanel;
  }


  public void setSupportIconToDelete(DrawableGraphElement elt,
                                     FaderIconHitContext context)
  {
    _currentElt = elt;
    _currentContext = context;
  }


  public void actionPerformed(ActionEvent e) {
    if (!(_currentElt == null || _currentContext == null)) {
      /**
       * For now, just set type visibility to false
       */
      TypeInfoAdapter typeInfo =
        TypeInfoAdapter.getTypeInfoAdapter(_currentElt);
      typeInfo.setTypeVisibility(_currentContext.getIndex(), false);
      if (_graphPanel != null) {
        _graphPanel.paintImmediately();
      }
    }
  }

} // end class "SupportIconDeleteAction"


