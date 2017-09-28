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

package edu.stanford.smi.protege.ui;


import java.awt.*;
import java.awt.event.*;
import java.util.*;
import javax.swing.*;
import javax.swing.table.*;
import edu.stanford.smi.protege.action.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.resource.*;
import edu.stanford.smi.protege.util.*;
import edu.stanford.smi.protege.widget.*;

class ConfigureJournalingPanel extends AbstractValidatableComponent {

    private Project _project;
    private JCheckBox _enabledCheckBox;
    private JTextField _userNameField;
    private JTextField _logFileField;

    public ConfigureJournalingPanel(Project project) {
        _project = project;
        setLayout(new GridLayout(6, 1));
        add(createEnabledCheckBox(project));
        add(createUserNameField());
        add(createLogFileField(project));
    }

    private JComponent createEnabledCheckBox(Project p) {
        _enabledCheckBox = ComponentFactory.createCheckBox();
        _enabledCheckBox.setText("Enabled");
        _enabledCheckBox.setSelected(p.isJournalingEnabled());
        return _enabledCheckBox;
    }

    private JComponent createLogFileField(Project p) {
        _logFileField = ComponentFactory.createTextField();
        _logFileField.setText(p.getJournalFile());
        _logFileField.setEditable(false);
        return new LabeledComponent("Journal File", _logFileField);
    }

    private JComponent createUserNameField() {
        _userNameField = ComponentFactory.createTextField();
        _userNameField.setText(Journal.getUserName());
        return new LabeledComponent("User", _userNameField);
    }

    public void saveContents() {
        Journal.setUserName(_userNameField.getText());
        _project.setJournalingEnabled(_enabledCheckBox.isSelected());
    }

    public boolean validateContents() {
        return true;
    }
}
