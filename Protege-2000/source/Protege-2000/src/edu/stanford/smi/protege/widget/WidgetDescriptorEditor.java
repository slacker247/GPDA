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
import java.util.*;
import javax.swing.*;
import javax.swing.table.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.ui.*;
import edu.stanford.smi.protege.util.*;

class WidgetDescriptorEditor extends DefaultCellEditor {

    private FormWidget _widget;

    public WidgetDescriptorEditor(FormWidget widget) {
        super(new JComboBox());
        _widget = widget;
        getComboBox().setRenderer(new WidgetClassNameRenderer());
    }

    private JComboBox getComboBox() {
        return (JComboBox) getComponent();
    }

    private ComboBoxModel getComboBoxModel(Cls cls, Slot slot) {
        DefaultComboBoxModel model = new DefaultComboBoxModel();
        model.addElement(WidgetClassNameRenderer.NONE);
        Project p = _widget.getProject();
        Iterator i = p.getSuitableWidgetClassNames(cls, slot, null).iterator();
        while (i.hasNext()) {
            model.addElement(i.next());
        }
        return model;
    }

    public Component getTableCellEditorComponent(JTable table, Object value, boolean isSelected, int row, int column) {
        String s = (String) value;
        JComboBox box = getComboBox();
        Slot slot = (Slot) table.getModel().getValueAt(row, 0);
        box.setModel(getComboBoxModel(_widget.getCls(), slot));
        box.setSelectedItem(value);
        return getComponent();
    }
}
