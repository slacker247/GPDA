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
import java.awt.event.*;
import java.util.*;
import javax.swing.*;
import javax.swing.event.*;
import edu.stanford.smi.protege.action.*;
import edu.stanford.smi.protege.resource.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.ui.*;
import edu.stanford.smi.protege.util.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class ClsesTab extends AbstractTabWidget {
    private ClsesPanel _clsesPanel;
    private ClsInverseRelationshipPanel _inverseRelationshipPanel;
    private InstanceDisplay _instanceDisplay;

    protected JComponent createClsDisplay() {
        _instanceDisplay = new InstanceDisplay(getProject());
        return _instanceDisplay;
    }

    protected ClsesPanel createClsesPanel() {
        ClsesPanel panel = new ClsesPanel(getProject());
        panel.addSelectionListener(new SelectionListener() {
            public void selectionChanged(SelectionEvent event) {
                transmitSelection();
            }
        });
        return panel;
    }

    protected JComponent createClsesSplitter() {
        JSplitPane pane = createTopBottomSplitPane("ClsesTab.left.top_bottom", 400);
        _clsesPanel = createClsesPanel();
        pane.setTopComponent(_clsesPanel);
        _inverseRelationshipPanel = createInverseRelationshipPanel();
        pane.setBottomComponent(_inverseRelationshipPanel);
        return pane;
    }

    protected ClsInverseRelationshipPanel createInverseRelationshipPanel() {
        final ClsInverseRelationshipPanel panel = new ClsInverseRelationshipPanel(getProject());
        panel.addSelectionListener(new SelectionListener() {
            public void selectionChanged(SelectionEvent event) {
                Collection selection = panel.getSelection();
                if (selection.size() == 1) {
                    Cls cls = (Cls) CollectionUtilities.getFirstItem(selection);
                    _clsesPanel.setDisplayParent(cls);
                }
            }
        });
        return panel;
    }

    private JComponent createMainSplitter() {
        JSplitPane pane = createLeftRightSplitPane("ClsesTab.left_right", 250);
        pane.setLeftComponent(createClsesSplitter());
        pane.setRightComponent(createClsDisplay());
        return pane;
    }

    public JTree getClsTree() {
        return _clsesPanel.getClsesTree();
    }

    public void initialize() {
        setIcon(Icons.getClsesIcon());
        setLabel("Classes");
        setShortDescription("Domain Ontology");
        add(createMainSplitter());
        setInitialSelection();
    }

    public void setFinderComponent(JComponent c) {
        _clsesPanel.setFinderComponent(c);
    }

    private void setInitialSelection() {
        if (_clsesPanel != null) {
            transmitSelection();
        }
    }

    protected void transmitSelection() {
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
        _instanceDisplay.setInstance(selectedInstance);
    }
}
