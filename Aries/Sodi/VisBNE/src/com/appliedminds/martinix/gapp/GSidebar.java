/* -*- Mode: Java; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*-*/
package com.appliedminds.martinix.gapp;


import java.awt.Dimension;
import java.awt.Component;
import javax.swing.JSplitPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.ImageIcon;

import com.appliedminds.core.widgets.ExpandablePanel;
import com.appliedminds.martini.GraphPanel;


/**
 * The sidebar used by the GAppFrame.
 *
 * @author daepark@apmindsf.com
 */
public class GSidebar extends JSplitPane implements GAppFrameListener
{

  private GAppFrame _app = null;

  private GFilterTable _edgeFilterTable;
  private GFilterTable _nodeFilterTable;

  public GSidebar(GAppFrame app,
                  GFilterTable nodeFilter,
                  GFilterTable edgeFilter)
  {
    super(JSplitPane.VERTICAL_SPLIT);

    _app = app;

    //
    // set up edge filter table
    //
    if (edgeFilter != null) {
      _edgeFilterTable = edgeFilter;
      _edgeFilterTable.setPreferredScrollableViewportSize(new Dimension(100, 100));
      ImageIcon relTypeClosed = _app.getIconAndCursorLoader().loadImageIcon("com/appliedminds/martinix/gapp/resources/relationship_types_closed.png");
      ImageIcon relTypeOpened = _app.getIconAndCursorLoader().loadImageIcon("com/appliedminds/martinix/gapp/resources/relationship_types_open.png");
      ExpandablePanel edgeFilterPanel =
        new ExpandablePanel(ExpandablePanel.createButton(relTypeClosed),
                            ExpandablePanel.createButton(relTypeOpened),
                            new JScrollPane(_edgeFilterTable.getComponent()));

      setTopComponent(edgeFilterPanel);
    }
    else {
      // Just use ablank panel if the edge table is null.
      setTopComponent(new JPanel());
    }

    //
    // set up node filter table
    //
    if (nodeFilter != null) {
      _nodeFilterTable = nodeFilter;
      _nodeFilterTable.setPreferredScrollableViewportSize(new Dimension(100, 100));
      ImageIcon nodeTypeClosed = _app.getIconAndCursorLoader().loadImageIcon("com/appliedminds/martinix/gapp/resources/node_types_closed.png");
      ImageIcon nodeTypeOpened = _app.getIconAndCursorLoader().loadImageIcon("com/appliedminds/martinix/gapp/resources/node_types_open.png");
      ExpandablePanel nodeFilterPanel =
        new ExpandablePanel(ExpandablePanel.createButton(nodeTypeClosed),
                            ExpandablePanel.createButton(nodeTypeOpened),
                            new JScrollPane(_nodeFilterTable.getComponent()));

      setBottomComponent(nodeFilterPanel);
    }
    else {
      // Just use a blank panel if the nodetable is null.
      setBottomComponent(new JPanel());
    }
  }


  public void gAppFrameGraphUpdated(GraphPanel graphPanel) {
    // sync the filter tables with the new graph
    if (_edgeFilterTable != null) {
      _edgeFilterTable.syncWithGraphPanel(graphPanel);
    }
    if (_nodeFilterTable != null) {
      _nodeFilterTable.syncWithGraphPanel(graphPanel);
    }
  }


} // end class GSidebar
