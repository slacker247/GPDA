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

class ClsValuesComponent extends AbstractValuesComponent implements Selectable {
    private Project _project;
    private SelectableList _list;
    private AllowableAction _addAction;
    private AllowableAction _removeAction;

    public ClsValuesComponent(Project p) {
        _project = p;
        _list = ComponentFactory.createSelectableList(null, true);
        _list.setCellRenderer(FrameRenderer.createInstance());
        _list.addListSelectionListener(new ListSelectionListenerAdapter(this));
        LabeledComponent c = new LabeledComponent("Allowed Parents", new JScrollPane(_list));
        c.addHeaderButton(getViewAction());
        c.addHeaderButton(getAddAction());
        c.addHeaderButton(getRemoveAction());
        add(c);
    }

    public void clearSelection() {
        _list.clearSelection();
    }

    private Action getAddAction() {
        _addAction = new AddAction("Add Class") {
            public void onAdd() {
                Collection clses = DisplayUtilities.pickClses(_list, getKnowledgeBase());
                ComponentUtilities.addUniqueListValues(_list, clses);
                valueChanged();
            }
        };
        return _addAction;
    }

    private Action getRemoveAction() {
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
        Collection values = ComponentUtilities.getListValues(_list);
        return ValueTypeConstraint.getValues(ValueType.CLS, values);
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
        Collection allowedParents = ValueTypeConstraint.getAllowedClses(values);
        ComponentUtilities.setListValues(_list, allowedParents);
    }
}
