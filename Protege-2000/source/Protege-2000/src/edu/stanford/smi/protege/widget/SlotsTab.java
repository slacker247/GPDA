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
import edu.stanford.smi.protege.resource.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.ui.*;
import edu.stanford.smi.protege.util.*;

/**
 * The standard slot tab.  This tab displays slots and subslots in a tree and allows
 * the user to edit top-level slots.
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class SlotsTab extends AbstractTabWidget {
    private SubslotPane _slotsPanel;
    private SlotClsesPanel _slotClsesPanel;
    private InstanceDisplay _slotDisplay;
    private JSplitPane _mainSplitter;
    private JSplitPane _clsesSplitter;

    private JComponent createMainSplitter() {
        _mainSplitter = createLeftRightSplitPane("SlotsTab.left_right", 250);
        _mainSplitter.setLeftComponent(createSlotsSplitter());
        _mainSplitter.setRightComponent(createSlotDisplay());
        return _mainSplitter;
    }

    private JComponent createSlotClsesPanel() {
        _slotClsesPanel = new SlotClsesPanel(getProject());
        return _slotClsesPanel;
    }

    private JComponent createSlotDisplay() {
        _slotDisplay = new InstanceDisplay(getProject());
        return _slotDisplay;
    }

    private JComponent createSlotsPanel() {
        _slotsPanel = new SubslotPane(getProject());
        _slotsPanel.addSelectionListener(
            new SelectionListener() {
                public void selectionChanged(SelectionEvent event) {
                    transmitSelection();
                }
            }
        );
        return _slotsPanel;
    }

    private JComponent createSlotsSplitter() {
        _clsesSplitter = createTopBottomSplitPane("SlotTab.left.top_bottom", 400);
        _clsesSplitter.setTopComponent(createSlotsPanel());
        _clsesSplitter.setBottomComponent(createSlotClsesPanel());
        return _clsesSplitter;
    }

    public void initialize() {
        setIcon(Icons.getSlotsIcon());
        setLabel("Slots");
        add(createMainSplitter());
        transmitSelection();
    }

    private void transmitSelection() {
        Slot selection = (Slot) CollectionUtilities.getFirstItem(_slotsPanel.getSelection());
        _slotClsesPanel.setSlot(selection);
        _slotDisplay.setInstance(selection);
    }
}
