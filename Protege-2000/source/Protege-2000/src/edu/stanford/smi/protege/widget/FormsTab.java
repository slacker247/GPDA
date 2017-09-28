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
 *  Tab to display class forms and allow the user to edit the forms
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class FormsTab extends AbstractTabWidget {
    private FormsPanel _formsPanel;
    private FormDisplay _formDisplay;

    private JComponent createFormDisplay() {
        _formDisplay = new FormDisplay(getProject());
        return _formDisplay;
    }

    private JComponent createFormsWidget() {
        _formsPanel = new FormsPanel(getProject());
        _formsPanel.addSelectionListener(
            new SelectionListener() {
                public void selectionChanged(SelectionEvent event) {
                    transmitSelection();
                }
            }
        );
        return _formsPanel;
    }

    private JComponent createMainSplitter() {
        JSplitPane pane = createLeftRightSplitPane("FormsTab.left_right", 250);
        pane.setLeftComponent(createFormsWidget());
        pane.setRightComponent(createFormDisplay());
        return pane;
    }

    public void initialize() {
        setIcon(Icons.getFormsIcon());
        setLabel("Forms");
        add(createMainSplitter());
        transmitSelection();
    }

    private void transmitSelection() {
        Collection selection = _formsPanel.getSelection();
        if (selection.size() == 1) {
            Cls cls = (Cls) CollectionUtilities.getFirstItem(selection);
            _formDisplay.setWidgetCls(cls);
        } else {
            _formDisplay.setWidgetCls(null);
        }
    }
}
