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

// java
import java.awt.*;
import java.awt.event.*;
import java.util.*;
import javax.swing.*;
import javax.swing.table.*;

// protege
import edu.stanford.smi.protege.action.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.resource.*;
import edu.stanford.smi.protege.util.*;
import edu.stanford.smi.protege.widget.*;

/**
 *
 * @author Ray Fergerson
 * @author Jennifer Vendetti
 */
class ConfigureOptionsPanel extends AbstractValidatableComponent {

    private Project _project;
    private JCheckBox _hiddenClassesComponent;
    private JCheckBox _abstractClassIconComponent;
    private JCheckBox _multiParentClassIconComponent;
    private JCheckBox _confirmOnRemoveComponent;
    private JCheckBox _isEditableComponent;
    private JCheckBox _updateModificationSlotsComponent;
    private JCheckBox showWelcomeDialog;

    public ConfigureOptionsPanel(Project project) {
        this._project = project;
        setLayout(new FlowLayout(FlowLayout.LEFT));
        JComponent c = new JPanel(new GridLayout(7, 1));
        c.add(createHiddenClassesComponent());
        c.add(createAbstractClassIconComponent());
        c.add(createMultiparentClassIconComponent());
        c.add(createConfirmOnRemoveComponent());
        c.add(createIsEditableComponent());
        c.add(createUpdateModificationSlotsComponent());

        boolean b = ApplicationProperties.getWelcomeDialogShow();
        showWelcomeDialog = new JCheckBox("Show Welcome Dialog on Start-up", b);
        c.add(showWelcomeDialog);
        add(c);
    }

    private JComponent createAbstractClassIconComponent() {
        _abstractClassIconComponent = ComponentFactory.createCheckBox("Display abstract class icon");
        setValue(_abstractClassIconComponent, _project.getDisplayAbstractClassIcon());
        return _abstractClassIconComponent;
    }

    private JComponent createConfirmOnRemoveComponent() {
        _confirmOnRemoveComponent = ComponentFactory.createCheckBox("Display confirmation dialog on remove");
        setValue(_confirmOnRemoveComponent, _project.getDisplayConfirmationOnRemove());
        return _confirmOnRemoveComponent;
    }

    private JComponent createHiddenClassesComponent() {
        _hiddenClassesComponent = ComponentFactory.createCheckBox("Display hidden classes");
        setValue(_hiddenClassesComponent, _project.getDisplayHiddenClasses());
        return _hiddenClassesComponent;
    }

    private JComponent createIsEditableComponent() {
        _isEditableComponent = ComponentFactory.createCheckBox("Allow knowledge-base changes");
        setValue(_isEditableComponent, !_project.isReadonly());
        return _isEditableComponent;
    }

    private JComponent createMultiparentClassIconComponent() {
        _multiParentClassIconComponent = ComponentFactory.createCheckBox("Display multi-parent class icon");
        setValue(_multiParentClassIconComponent, _project.getDisplayMultiParentClassIcon());
        return _multiParentClassIconComponent;
    }

    private JComponent createUpdateModificationSlotsComponent() {
        _updateModificationSlotsComponent = ComponentFactory.createCheckBox("Update modification slots and facets");
        setValue(_updateModificationSlotsComponent, _project.getUpdateModificationSlots());
        return _updateModificationSlotsComponent;
    }

    private static boolean getValue(JCheckBox box) {
        return box.isSelected();
    }

    public void saveContents() {
        _project.setDisplayHiddenClasses(getValue(_hiddenClassesComponent));
        _project.setDisplayAbstractClassIcon(getValue(_abstractClassIconComponent));
        _project.setDisplayMultiParentClassIcon(getValue(_multiParentClassIconComponent));
        _project.setDisplayConfirmationOnRemove(getValue(_confirmOnRemoveComponent));
        _project.setIsReadonly(!getValue(_isEditableComponent));
        _project.setUpdateModificationSlots(getValue(_updateModificationSlotsComponent));
        Boolean b = new Boolean(showWelcomeDialog.isSelected());
        ApplicationProperties.setWelcomeDialogShow(b);
    }

    private static void setValue(JCheckBox box, boolean value) {
        box.setSelected(value);
    }

    public boolean validateContents() {
        return true;
    }
}
