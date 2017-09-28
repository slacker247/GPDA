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
import edu.stanford.smi.protege.event.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.ui.*;
import edu.stanford.smi.protege.util.*;

/**
 *  Description of the Class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class InverseSlotWidget extends AbstractSlotWidget {
    private JList _list;
    private AllowableAction _viewAction;
    private AllowableAction _createAction;
    private AllowableAction _addAction;
    private AllowableAction _removeAction;

    private FrameListener _frameListener = new FrameAdapter() {
        public void ownSlotValueChanged(FrameEvent event) {
            updateWidget();
        }
    };

    private Slot createInverseSlot() {
        Slot forwardSlot = (Slot) getInstance();
        String slotName = "inverse_of_" + forwardSlot.getName();
        Slot slot = getKnowledgeBase().createSlot(slotName);
        slot.setValueType(ValueType.INSTANCE);
        slot.setAllowsMultipleValues(true);
        setInverseSlot(slot);
        return slot;
    }

    private JList createList() {
        JList list = ComponentFactory.createSingleItemList(getViewAction());
        list.setCellRenderer(FrameRenderer.createInstance());
        return list;
    }

    private Action getAddAction() {
        if (_addAction == null) {
            _addAction = new AddAction("Add inverse slot") {
                public void onAdd() {
                    selectSlotToAdd();
                }
            };
        }
        return _addAction;
    }

    private Action getCreateAction() {
        if (_createAction == null) {
            _createAction =
                new CreateAction("Create new inverse slot") {
                    public void onCreate() {
                        Slot slot = createInverseSlot();
                        getProject().show(slot);
                    }
                }
            ;
        }
        return _createAction;
    }

    private Collection getPossibleInverses() {
        Collection possibleInverses = new ArrayList();
        Iterator i = getKnowledgeBase().getSlots().iterator();
        while (i.hasNext()) {
            Slot slot = (Slot) i.next();
            ValueType type = slot.getValueType();
            Slot inverseSlot = slot.getInverseSlot();
            if (!slot.isSystem() && type == ValueType.INSTANCE && inverseSlot == null) {
                possibleInverses.add(slot);
            }
        }
        return possibleInverses;
    }

    private Action getRemoveAction() {
        if (_removeAction == null) {
            _removeAction =
                new RemoveAction("Remove inverse slot", this) {
                    public void onRemove(Object o) {
                        setInverseSlot(null);
                    }
                }
            ;
        }
        return _removeAction;
    }

    public Collection getSelection() {
        return getValues();
    }

    public Collection getValues() {
        return ComponentUtilities.getListValues(_list);
    }

    private Action getViewAction() {
        if (_viewAction == null) {
            _viewAction =
                new ViewAction("View inverse slot", this) {
                    public void onView(Object o) {
                        Slot slot = (Slot) o;
                        getProject().show(slot);
                    }
                }
            ;
        }
        return _viewAction;
    }

    public void initialize() {
        _list = createList();
        LabeledComponent c = new LabeledComponent(getLabel(), _list);
        c.addHeaderButton(getViewAction());
        c.addHeaderButton(getCreateAction());
        c.addHeaderButton(getAddAction());
        c.addHeaderButton(getRemoveAction());
        if (isSlotAtCls()) {
            _createAction.setAllowed(false);
            _addAction.setAllowed(false);
            _removeAction.setAllowed(false);
        }
        add(c);
        setPreferredColumns(2);
        setPreferredRows(1);
    }

    public static boolean isSuitable(Cls cls, Slot slot, Facet facet) {
        return slot.getName().equals(Model.Slot.INVERSE);
    }

    private void selectSlotToAdd() {
        Collection possibleInverses = getPossibleInverses();
        if (possibleInverses.isEmpty()) {
            String text = "There are no existing slots which can be used as an inverse.";
            ModalDialog.showMessageDialog(this, text);
        } else {
            Slot slot = DisplayUtilities.pickSlot(this, possibleInverses);
            if (slot != null) {
                setInverseSlot(slot);
            }
        }
    }

    public void setEditable(boolean b) {
        updateWidget();
    }

    public void setInstance(Instance newInstance) {
        Instance oldInstance = getInstance();
        if (oldInstance != null && !isSlotAtCls()) {
            oldInstance.removeFrameListener(_frameListener);
        }
        super.setInstance(newInstance);
        if (newInstance != null && !isSlotAtCls()) {
            newInstance.addFrameListener(_frameListener);
        }
    }

    private void setInverseSlot(Slot slot) {
        Collection values = CollectionUtilities.createCollection(slot);
        ComponentUtilities.setListValues(_list, values);
        valueChanged();
    }

    public void setValues(Collection c) {
        ComponentUtilities.setListValues(_list, c);
    }

    private void updateWidget() {
        Instance instance = getInstance();
        boolean editable = !isSlotAtCls() && instance.isEditable();
        if (editable && instance instanceof Slot) {
            ValueType type = ((Slot) instance).getValueType();
            editable = (type == ValueType.INSTANCE) || (type == ValueType.CLS);
        }
        _createAction.setAllowed(editable);
        _addAction.setAllowed(editable);
        _removeAction.setAllowed(editable);

    }
}
