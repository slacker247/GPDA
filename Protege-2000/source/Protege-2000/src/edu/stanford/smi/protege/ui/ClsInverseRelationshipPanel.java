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


import java.util.*;
import java.awt.*;
import javax.swing.*;
import edu.stanford.smi.protege.action.*;
import edu.stanford.smi.protege.event.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.resource.*;
import edu.stanford.smi.protege.ui.*;
import edu.stanford.smi.protege.util.*;

/**
 * This panel displays the superclasses of a given class and allows the user to
 * add or remove a superclass.
 *
 * The original intent of the class was to display
 * an arbitrary (inverse relationship) but the only relationship implemented at
 * the moment if the "inverse of subclass".
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class ClsInverseRelationshipPanel extends SelectableContainer {
    private AllowableAction _addAction;
    private AllowableAction _removeAction;
    private Project _project;
    private SelectableList _list;
    private Cls _viewedCls;

    private ClsListener _clsListener = new ClsAdapter() {
        public void directSuperclassAdded(ClsEvent event) {
            ComponentUtilities.addListValue(_list, event.getSuperclass());
        }

        public void directSuperclassRemoved(ClsEvent event) {
            boolean wasEnabled = setNotificationsEnabled(false);
            ComponentUtilities.removeListValue(_list, event.getSuperclass());
            setNotificationsEnabled(wasEnabled);
        }
    };

    public ClsInverseRelationshipPanel(Project p) {
        _project = p;
        setLayout(new BorderLayout());
        _list = createList();
        LabeledComponent c = new LabeledComponent("Superclasses", ComponentFactory.createScrollPane(_list));
        c.addHeaderButton(getAddParentAction());
        c.addHeaderButton(getRemoveParentAction());
        add(c);
        setSelectable(_list);
    }

    private boolean canBeSuperclass(Cls cls) {
        boolean canBeSuperclass = true;
        if (cls == _viewedCls || cls.hasSuperclass(_viewedCls) || _viewedCls.hasSuperclass(cls)) {
            canBeSuperclass = false;
            ModalDialog.showMessageDialog(this, cls.getName() + " can not be a superclass of " + _viewedCls.getName());
        }
        return canBeSuperclass;
    }

    private SelectableList createList() {
        SelectableList list = new SelectableList();
        list.setCellRenderer(FrameRenderer.createInstance());
        return list;
    }

    private void doRemoveSuperclasses(Collection superclasses) {
        Iterator i = superclasses.iterator();
        while (i.hasNext()) {
            Cls superclass = (Cls) i.next();
            _viewedCls.removeDirectSuperclass(superclass);
        }
    }

    private Action getAddParentAction() {
        _addAction = new AddAction("Add parent to selected classes in tree") {
            public void onAdd() {
                if (_viewedCls != null) {
                    Iterator i = DisplayUtilities.pickClses(ClsInverseRelationshipPanel.this, getKnowledgeBase()).iterator();
                    while (i.hasNext()) {
                        Cls parent = (Cls) i.next();
                        if (canBeSuperclass(parent)) {
                            _viewedCls.addDirectSuperclass(parent);
                        }
                    }
                }
            }
        };
        return _addAction;
    }

    private KnowledgeBase getKnowledgeBase() {
        return _project.getKnowledgeBase();
    }

    private Action getRemoveParentAction() {
        _removeAction = new RemoveAction("Remove selected parent from selected class in tree", this) {
            public void onRemove() {
                if (_viewedCls != null) {
                    removeSelectedSuperclasses();
                }
            }
        };
        return _removeAction;
    }

    public void onSelectionChange() {
        updateButtons();
    }

    private void reload() {
        Collection values = (_viewedCls == null) ? Collections.EMPTY_LIST : _viewedCls.getDirectSuperclasses();
        ComponentUtilities.setListValues(_list, values);
    }

    private void removeSelectedSuperclasses() {
        Collection selection = new ArrayList(getSelection());
        Set allSuperclasses = new HashSet(_viewedCls.getDirectSuperclasses());
        if (selection.size() != allSuperclasses.size()) {
            allSuperclasses.removeAll(selection);
            Cls newSelection = (Cls) CollectionUtilities.getFirstItem(allSuperclasses);
            setSelection(newSelection);
            doRemoveSuperclasses(selection);
        }
    }

    public void setCls(Cls newCls, Cls parent) {
        // Log.enter(this, "setCls", newCls, parent);
        if (newCls != _viewedCls) {
            if (_viewedCls != null) {
                _viewedCls.removeClsListener(_clsListener);
            }
            _viewedCls = newCls;
            if (_viewedCls != null) {
                _viewedCls.addClsListener(_clsListener);
            }
            reload();
        }
        boolean wasPosting = setNotificationsEnabled(false);
        setSelection(parent);
        setNotificationsEnabled(wasPosting);

        updateButtons();
    }

    private void setSelection(Cls parent) {
        _list.setSelectedValue(parent, true);
    }

    private void updateButtons() {
        boolean canModify = (_viewedCls != null) && _viewedCls.isEditable();
        _addAction.setEnabled(canModify);
        boolean canRemove = canModify && getSelection().size() < _viewedCls.getDirectSuperclassCount();
        _removeAction.setAllowed(canRemove);
    }
}
