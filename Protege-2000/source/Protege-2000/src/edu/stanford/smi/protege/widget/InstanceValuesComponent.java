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

class InstanceValuesComponent extends AbstractValuesComponent implements Selectable {
    private Project _project;
    private SelectableList _list;
    private AllowableAction _addAction;
    private AllowableAction _removeAction;

    public InstanceValuesComponent(Project project) {
        _project = project;
        _list = ComponentFactory.createSelectableList(null, true);
        _list.setCellRenderer(FrameRenderer.createInstance());
        _list.addListSelectionListener(new ListSelectionListenerAdapter(this));
        LabeledComponent c = new LabeledComponent("Allowed Classes", new JScrollPane(_list));
        c.addHeaderButton(getViewAction());
        c.addHeaderButton(getAddClsesAction());
        c.addHeaderButton(getRemoveClsesAction());
        add(c);
    }

    public void clearSelection() {
        _list.clearSelection();
    }

    private Action getAddClsesAction() {
        _addAction = new AddAction("Add Classes") {
            public void onAdd() {
                Collection clses = DisplayUtilities.pickClses(_list, getKnowledgeBase());
                ComponentUtilities.addUniqueListValues(_list, clses);
                valueChanged();
            }
        };
        return _addAction;
    }

    private Action getRemoveClsesAction() {
        _removeAction =
            new RemoveAction("Remove Selected Classes", this) {
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
        Collection clses = ComponentUtilities.getListValues(_list);
        return ValueTypeConstraint.getValues(ValueType.INSTANCE, clses);
    }

    private Action getViewAction() {
        return new ViewAction("View Class", this) {
            public void onView(Object o) {
                Cls cls = (Cls) o;
                _project.show(cls);
            }
        };
    }

    public void setEditable(boolean b) {
        _addAction.setAllowed(b);
        _removeAction.setAllowed(b);
    }

    public void setValues(Collection values) {
        Collection clses = ValueTypeConstraint.getAllowedClses(values);
        ComponentUtilities.setListValues(_list, clses);
    }
}
