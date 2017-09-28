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
import java.util.List;
import javax.swing.*;
import edu.stanford.smi.protege.action.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.util.*;
import edu.stanford.smi.protege.ui.*;

class SymbolValuesComponent extends AbstractValuesComponent implements Selectable {

    private SelectableList _list;
    private AllowableAction _createAction;
    private AllowableAction _editAction;
    private AllowableAction _removeAction;

    public SymbolValuesComponent() {
        Action editAction = getEditAction();
        _list = ComponentFactory.createSelectableList(editAction, true);
        _list.addListSelectionListener(new ListSelectionListenerAdapter(this));
        LabeledComponent c = new LabeledComponent("Allowed Values", new JScrollPane(_list));
        c.addHeaderButton(editAction);
        c.addHeaderButton(getCreateAction());
        c.addHeaderButton(getRemoveAction());
        add(c);
    }

    public void clearSelection() {
        _list.clearSelection();
    }

    private Action getCreateAction() {
        _createAction =
            new CreateAction("Create Symbol Value") {
                public void onCreate() {
                    String s = DisplayUtilities.editString(_list, "Create Symbol", null, new SymbolValidator());
                    if (s != null && s.length() > 0) {
                        ComponentUtilities.addSelectedListValue(_list, s);
                        valueChanged();
                    }
                }
            }
        ;
        return _createAction;
    }

    private Action getEditAction() {
        _editAction =
            new ViewAction("Edit Symbol Value", this) {
                public void onView(Object o) {
                    String s = DisplayUtilities.editString(_list, "Edit Symbol", (String) o, new SymbolValidator());
                    if (s != null) {
                        ComponentUtilities.replaceListValue(_list, o, s);
                        valueChanged();
                    }
                }
            }
        ;
        return _editAction;
    }

    private Action getRemoveAction() {
        _removeAction =
            new RemoveAction("Remove Symbol Value", this) {
                public void onRemove(Object o) {
                    ComponentUtilities.removeListValue(_list, o);
                    valueChanged();
                }
            }
        ;
        return _removeAction;
    }

    public Collection getSelection() {
        return _list.getSelection();
    }

    public Collection getValues() {
        Collection values = ComponentUtilities.getListValues(_list);
        return ValueTypeConstraint.getValues(ValueType.SYMBOL, values);
    }

    public void setEditable(boolean b) {
        _createAction.setAllowed(b);
        _editAction.setAllowed(b);
        _removeAction.setAllowed(b);
    }

    public void setValues(Collection values) {
        Collection allowedValues = ValueTypeConstraint.getAllowedValues(values);
        ComponentUtilities.setListValues(_list, allowedValues);
    }
}
