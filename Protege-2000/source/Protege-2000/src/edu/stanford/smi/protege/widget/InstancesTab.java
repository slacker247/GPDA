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
public class InstancesTab extends AbstractTabWidget {
    private InstanceDisplay _instanceDisplay;
    private InstanceClsesPanel _clsesPanel;
    private DirectInstancesList _directInstancesList;

    private JComponent createClsesPanel() {
        _clsesPanel = new InstanceClsesPanel(getProject());
        _clsesPanel.addSelectionListener(new SelectionListener() {
            public void selectionChanged(SelectionEvent event) {
                transmitSelection();
            }
        });
        return _clsesPanel;
    }

    private JComponent createClsSplitter() {
        JSplitPane pane = createLeftRightSplitPane("InstancesTab.left_right", 250);
        pane.setLeftComponent(createClsesPanel());
        pane.setRightComponent(createInstanceSplitter());
        return pane;
    }

    private JComponent createDirectInstancesList() {
        _directInstancesList = new DirectInstancesList(getProject());
        _directInstancesList.addSelectionListener(new SelectionListener() {
            public void selectionChanged(SelectionEvent event) {
                Collection selection = _directInstancesList.getSelection();
                Instance selectedInstance;
                if (selection.size() == 1) {
                    selectedInstance = (Instance) CollectionUtilities.getFirstItem(selection);
                } else {
                    selectedInstance = null;
                }
                // itsClsList.clearSelection();
                _instanceDisplay.setInstance(selectedInstance);
            }
        });
        return _directInstancesList;
    }

    private JComponent createInstanceDisplay() {
        _instanceDisplay = new InstanceDisplay(getProject());
        return _instanceDisplay;
    }

    private JComponent createInstancesPanel() {
        JPanel panel = ComponentFactory.createPanel();
        panel.setLayout(new BorderLayout());
        // panel.add(createClsDisplay(), BorderLayout.NORTH);
        panel.add(createDirectInstancesList(), BorderLayout.CENTER);
        return panel;
    }

    private JComponent createInstanceSplitter() {
        JSplitPane pane = createLeftRightSplitPane("InstancesTab.right.left_right", 250);
        pane.setLeftComponent(createInstancesPanel());
        pane.setRightComponent(createInstanceDisplay());
        return pane;
    }

    public void initialize() {
        setIcon(Icons.getInstancesIcon());
        setLabel("Instances");
        add(createClsSplitter());
        transmitSelection();
        setupDragAndDrop();
    }

    protected void setSelectedCls(Cls cls) {
        _clsesPanel.setSelectedCls(cls);
    }

    protected void setSelectedInstance(Instance instance) {
        _clsesPanel.setSelectedCls(instance.getDirectType());
        _directInstancesList.setSelectedInstance(instance);
    }

    private void setupDragAndDrop() {
        DragSource.getDefaultDragSource().createDefaultDragGestureRecognizer(_directInstancesList.getDragComponent(),
                DnDConstants.ACTION_COPY_OR_MOVE, new InstancesTabDirectInstancesListDragSourceListener());
        new DropTarget(_clsesPanel.getDropComponent(), DnDConstants.ACTION_COPY_OR_MOVE, new InstanceClsesTreeTarget());
    }

    private void transmitSelection() {
        Collection selection = _clsesPanel.getSelection();
        _directInstancesList.setClses(selection);
        _directInstancesList.initializeSelection();
    }
}
