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

import java.util.*;
import javax.swing.*;
import javax.swing.event.*;
import edu.stanford.smi.protege.action.*;
import edu.stanford.smi.protege.event.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.ui.*;
import edu.stanford.smi.protege.util.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class InstanceFieldWidget extends AbstractSlotWidget {
    private JList _list;
    private Instance _instance;
    private AllowableAction _createAction;
    private AllowableAction _addAction;
    private AllowableAction _removeAction;
    private AllowableAction _deleteAction;

    private FrameListener _instanceListener = new FrameAdapter() {
        public void browserTextChanged(FrameEvent event) {
            _list.repaint();
        }
    };

    protected void addButton(LabeledComponent c, Action action, boolean defaultState) {
        if (action != null) {
            addButtonConfiguration(action, defaultState);
            if (displayButton(action)) {
                c.addHeaderButton(action);
            }
        }
    }

    protected void addButtons(LabeledComponent c) {
        addButton(c, getViewInstanceAction(), true);
        addButton(c, new ReferencersAction(this), false);
        addButton(c, getCreateInstanceAction(), true);
        addButton(c, getSelectInstanceAction(), true);
        addButton(c, getRemoveInstanceAction(), true);
        addButton(c, getDeleteInstancesAction(), false);
    }

    public JList createList() {
        JList list = ComponentFactory.createSingleItemList(getDoubleClickAction());
        list.setCellRenderer(FrameRenderer.createInstance());
        return list;
    }

    public void dispose() {
        if (_instance != null) {
            _instance.removeFrameListener(_instanceListener);
        }
        super.dispose();
    }

    protected Action getCreateInstanceAction() {
        _createAction = new CreateAction("Create Instance") {
            public void onCreate() {
                handleCreateAction();
            }
        };
        return _createAction;
    }

    protected Action getDeleteInstancesAction() {
        _deleteAction = new DeleteInstancesAction("Delete Instance", this);
        return _deleteAction;
    }

    protected Action getRemoveInstanceAction() {
        _removeAction = new RemoveAction("Remove Instance", this) {
            public void onRemove(Object o) {
                handleRemoveAction();
            }
        };
        return _removeAction;
    }

    protected Action getSelectInstanceAction() {
        _addAction = new AddAction("Add Instance") {
            public void onAdd() {
                handleAddAction();
            }
        };
        return _addAction;
    }

    public Collection getSelection() {
        return CollectionUtilities.createCollection(this._instance);
    }

    public Collection getValues() {
        return CollectionUtilities.createList(this._instance);
    }

    protected Action getViewInstanceAction() {
        return new ViewAction("View Instance", this) {
            public void onView(Object o) {
                handleViewAction((Instance) o);
            }
        };
    }

    protected void handleAddAction() {
        Collection clses = getCls().getTemplateSlotAllowedClses(getSlot());
        Instance instance = DisplayUtilities.pickInstance(InstanceFieldWidget.this, clses);
        if (instance != null) {
            setDisplayedInstance(instance);
        }
    }

    protected void handleCreateAction() {
        Collection clses = getCls().getTemplateSlotAllowedClses(getSlot());
        Cls cls = DisplayUtilities.pickConcreteCls(InstanceFieldWidget.this, clses);
        if (cls != null) {
            Instance instance = getKnowledgeBase().createInstance(null, cls);
            if (instance instanceof Cls) {
                Cls newcls = (Cls) instance;
                if (newcls.getDirectSuperclassCount() == 0) {
                    newcls.addDirectSuperclass(getKnowledgeBase().getRootCls());
                }
            }
            showInstance(instance);
            setDisplayedInstance(instance);
        }
    }

    protected void handleRemoveAction() {
        removeDisplayedInstance();
    }

    protected void handleViewAction(Instance instance) {
        showInstance((Instance) instance);
    }

    public void initialize() {
        _list = createList();
        LabeledComponent c = new LabeledComponent(getLabel(), _list);
        addButtons(c);
        add(c);
        setPreferredColumns(2);
        setPreferredRows(1);
    }

    public static boolean isSuitable(Cls cls, Slot slot, Facet facet) {
        boolean isSuitable;
        if (cls == null || slot == null) {
            isSuitable = false;
        } else {
            boolean isInstance = cls.getTemplateSlotValueType(slot) == ValueType.INSTANCE;
            boolean isMultiple = cls.getTemplateSlotAllowsMultipleValues(slot);
            isSuitable = isInstance && !isMultiple;
        }
        return isSuitable;
    }

    protected void removeDisplayedInstance() {
        replaceInstance(null);
        updateList();
        valueChanged();
    }

    protected void replaceInstance(Instance instance) {
        if (_instance != null) {
            _instance.removeFrameListener(_instanceListener);
        }
        _instance = instance;
        if (_instance != null) {
            _instance.addFrameListener(_instanceListener);
        }
        notifySelectionListeners();
    }

    protected void setDisplayedInstance(Instance instance) {
        replaceInstance(instance);
        updateList();
        valueChanged();
    }

    public void setEditable(boolean b) {
        setAllowed(_createAction, b);
        setAllowed(_addAction, b);
        setAllowed(_removeAction, b);
        setAllowed(_deleteAction, b);
    }

    public void setValues(Collection values) {
        Instance value = (Instance) CollectionUtilities.getFirstItem(values);
        replaceInstance(value);
        updateList();
    }

    protected void updateList() {
        ComponentUtilities.setListValues(_list, CollectionUtilities.createCollection(_instance));
    }
}
