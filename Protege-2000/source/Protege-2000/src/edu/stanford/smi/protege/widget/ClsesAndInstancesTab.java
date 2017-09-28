/*
 * The contents of this file are subject to the Mozilla Public License
 * Version 1.1 (the "License");  you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 * the specific language governing rights and limitations under the License.
 *
 * The Original Code is Protege-2000.
 *
 * The Initial Developer of the Original Code is Stanford University. Portions
 * created by Stanford University are Copyright (C) 2001.  All Rights Reserved.
 *
 * Protege-2000 was developed by Stanford Medical Informatics
 * (http://www.smi.stanford.edu) at the Stanford University School of Medicine
 * with support from the National Library of Medicine, the National Science
 * Foundation, and the Defense Advanced Research Projects Agency.  Current
 * information about Protege can be obtained at http://protege.stanford.edu
 *
 * Contributor(s):
 */

package edu.stanford.smi.protege.widget;


import java.awt.*;
import java.awt.dnd.*;
import java.awt.event.*;
import java.util.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.tree.*;
import edu.stanford.smi.protege.resource.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.ui.*;
import edu.stanford.smi.protege.util.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class ClsesAndInstancesTab extends AbstractTabWidget {
    private InstanceDisplay _instanceDisplay;
    private ClsesPanel _clsesPanel;
    private DirectInstancesList _directInstancesList;
    private SelectableList _clsList;
    private ClsInverseRelationshipPanel _inverseRelationshipPanel;
    private boolean _isUpdating;

    private JComponent createClsControlPanel() {
        JSplitPane pane = createTopBottomSplitPane("ClsesAndInstancesTab.left, top_bottom", 400);
        pane.setTopComponent(createClsesPanel());
        pane.setBottomComponent(createInverseRelationshipPanel());
        return pane;
    }

    private JComponent createClsDisplay() {
        _clsList = ComponentFactory.createSingleItemList(null);
        _clsList.setCellRenderer(FrameRenderer.createInstance());
        _clsList.addSelectionListener(
            new SelectionListener() {
                public void selectionChanged(SelectionEvent event) {
                    if (!_isUpdating) {
                        _isUpdating = true;
                        Collection selection = event.getSelectable().getSelection();
                        Instance firstSelection = (Instance) CollectionUtilities.getFirstItem(selection);
                        _directInstancesList.clearSelection();
                        _instanceDisplay.setInstance(firstSelection);
                        _isUpdating = false;
                    }
                }
            }
        );
        LabeledComponent c = new LabeledComponent("Class", _clsList);
        return c;
    }

    private JComponent createClsesPanel() {
        _clsesPanel = new ClsesPanel(getProject());
        FrameRenderer renderer = FrameRenderer.createInstance();
        renderer.setDisplayDirectInstanceCount(true);
        _clsesPanel.setRenderer(renderer);
        _clsesPanel.addSelectionListener(
            new SelectionListener() {
                public void selectionChanged(SelectionEvent event) {
                    transmitClsSelection();
                }
            }
        );
        return _clsesPanel;
    }

    private JComponent createClsSplitter() {
        JSplitPane pane = createLeftRightSplitPane("ClsesAndInstancesTab.left_right", 250);
        pane.setLeftComponent(createClsControlPanel());
        pane.setRightComponent(createInstanceSplitter());
        return pane;
    }

    private JComponent createDirectInstancesList() {
        _directInstancesList = new DirectInstancesList(getProject());
        _directInstancesList.addSelectionListener(
            new SelectionListener() {
                public void selectionChanged(SelectionEvent event) {
                    if (!_isUpdating) {
                        _isUpdating = true;
                        Collection selection = _directInstancesList.getSelection();
                        Instance selectedInstance;
                        if (selection.size() == 1) {
                            selectedInstance = (Instance) CollectionUtilities.getFirstItem(selection);
                        } else {
                            selectedInstance = null;
                        }
                        _clsList.clearSelection();
                        _instanceDisplay.setInstance(selectedInstance);
                        _isUpdating = false;
                    }
                }
            }
        );
        return _directInstancesList;
    }

    private JComponent createInstanceDisplay() {
        _instanceDisplay = new InstanceDisplay(getProject());
        return _instanceDisplay;
    }

    private JComponent createInstancesPanel() {
        JPanel panel = ComponentFactory.createPanel();
        panel.setLayout(new BorderLayout(10, 10));
        panel.add(createClsDisplay(), BorderLayout.NORTH);
        panel.add(createDirectInstancesList(), BorderLayout.CENTER);
        return panel;
    }

    private JComponent createInstanceSplitter() {
        JSplitPane pane = createLeftRightSplitPane("ClsesAndInstancesTab.right.left_right", 200);
        pane.setLeftComponent(createInstancesPanel());
        pane.setRightComponent(createInstanceDisplay());
        return pane;
    }

    private JComponent createInverseRelationshipPanel() {
        _inverseRelationshipPanel = new ClsInverseRelationshipPanel(getProject());
        _inverseRelationshipPanel.addSelectionListener(
            new SelectionListener() {
                public void selectionChanged(SelectionEvent event) {
                    Collection selection = _inverseRelationshipPanel.getSelection();
                    if (selection.size() == 1) {
                        Cls cls = (Cls) selection.iterator().next();
                        _clsesPanel.setDisplayParent(cls);
                    }
                }
            }
        );
        return _inverseRelationshipPanel;
    }

    public void initialize() {
        setIcon(Icons.getClsesAndInstancesIcon());
        setLabel("Classes & Instances");
        add(createClsSplitter());
        setupDragAndDrop();
        transmitClsSelection();
    }

    private void setupDragAndDrop() {
        DragSource.getDefaultDragSource().createDefaultDragGestureRecognizer(_directInstancesList,
                DnDConstants.ACTION_COPY_OR_MOVE, new ClsesAndInstancesTabDirectInstancesListDragSourceListener());
        new DropTarget(_clsesPanel.getDropComponent(), DnDConstants.ACTION_COPY_OR_MOVE, new InstanceClsesTreeTarget());
    }

    private void transmitClsSelection() {
        // Log.enter(this, "transmitSelection");
        Collection selection = _clsesPanel.getSelection();
        Instance selectedInstance = null;
        Cls selectedCls = null;
        Cls selectedParent = null;
        if (selection.size() == 1) {
            selectedInstance = (Instance) CollectionUtilities.getFirstItem(selection);
            if (selectedInstance instanceof Cls) {
                selectedCls = (Cls) selectedInstance;
                selectedParent = _clsesPanel.getDisplayParent();
            }
        }
        _inverseRelationshipPanel.setCls(selectedCls, selectedParent);
        _directInstancesList.setClses(selection);
        ComponentUtilities.setListValues(_clsList, selection);
        if (!selection.isEmpty()) {
            _clsList.setSelectedIndex(0);
        }
    }
}
