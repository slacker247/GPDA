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
import edu.stanford.smi.protege.event.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.resource.*;
import edu.stanford.smi.protege.ui.*;
import edu.stanford.smi.protege.util.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class ClsFieldWidget extends AbstractSlotWidget {
    private JList _list;
    private Instance _instance;
    private AllowableAction _addAction;
    private AllowableAction _removeAction;

    private FrameListener _instanceListener = new FrameAdapter() {
        public void browserTextChanged(FrameEvent event) {
        }
        public void ownSlotValueChanged(FrameEvent event) {
            repaint();
        }
        public void nameChanged(FrameEvent event) {
            repaint();
        }
    };

    public void addButton(LabeledComponent c, Action action) {
        addButtonConfiguration(action);
        if (displayButton(action)) {
            c.addHeaderButton(action);
        }
    }

    protected void addButtons(LabeledComponent c) {
        addButton(c, getViewClsAction());
        addButton(c, getSelectClsAction());
        addButton(c, getRemoveClsAction());
    }

    public JList createList() {
        JList list = ComponentFactory.createSingleItemList(getDoubleClickAction());
        list.setCellRenderer(FrameRenderer.createInstance());
        return list;
    }

    public void dispose() {
        super.dispose();
        if (this._instance != null) {
            this._instance.removeFrameListener(this._instanceListener);
        }
    }

    protected Action getRemoveClsAction() {
        _removeAction = new RemoveAction("Remove Class", this) {
            public void onRemove(Object o) {
                handleRemoveAction();
            }
        };
        return _removeAction;
    }

    protected Action getSelectClsAction() {
        _addAction = new AddAction("Add Class") {
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

    protected Action getViewClsAction() {
        return new ViewAction("View Class", this) {
            public void onView(Object o) {
                handleViewAction((Instance) o);
            }
        };
    }

    protected void handleAddAction() {
            Collection clses = getCls().getTemplateSlotAllowedParents(getSlot());
            Instance instance = DisplayUtilities.pickCls(ClsFieldWidget.this, clses);
            if (instance != null) {
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
            boolean isInstance = cls.getTemplateSlotValueType(slot) == ValueType.CLS;
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
        if (this._instance != null) {
            this._instance.removeFrameListener(this._instanceListener);
        }
        this._instance = instance;
        if (this._instance != null) {
            this._instance.addFrameListener(this._instanceListener);
        }
        notifySelectionListeners();
    }

    protected void setDisplayedInstance(Instance instance) {
        replaceInstance(instance);
        updateList();
        valueChanged();
    }

    public void setEditable(boolean b) {
        setAllowed(_addAction, b);
        setAllowed(_removeAction, b);
    }

    public void setValues(Collection values) {
        Instance value = (Instance) CollectionUtilities.getFirstItem(values);
        replaceInstance(value);
        updateList();
    }

    protected void updateList() {
        ComponentUtilities.setListValues(_list, CollectionUtilities.createCollection(this._instance));
    }
}
