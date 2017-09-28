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
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class SlotValuesWidget extends AbstractListWidget {
    private Action _viewAction;
    private AllowableAction _createAction;
    private AllowableAction _addAction;
    private Action _removeAction;
    private ValueType _previousType;

    private ClsListener _clsListener = new ClsAdapter() {
        public void templateFacetValueChanged(ClsEvent event) {
            super.templateFacetValueChanged(event);
            if (event.getFacet().getName().equals(Model.Facet.VALUE_TYPE)) {
                updateButtons();
            }
        }
    };
    private FrameListener _frameListener = new FrameAdapter() {
        public void ownSlotValueChanged(FrameEvent event) {
            super.ownSlotValueChanged(event);
            if (event.getSlot().getName().equals(Model.Slot.VALUE_TYPE)) {
                updateButtons();
            }
        }
    };

    private Collection addItems() {
        Collection items = null;
        ValueType type = getCurrentType();
        if (type == ValueType.CLS) {
            Collection c = getAllowedParents();
            items = DisplayUtilities.pickClses(this, c);
        } else if (type == ValueType.INSTANCE) {
            Collection c = getAllowedClses();
            items = DisplayUtilities.pickInstances(this, c);
        } else {
            Log.error("bad type", this, "addItem");
        }
        return items;
    }

    private Boolean editBoolean(Object o) {
        Collection c = Arrays.asList(new Object[] { "true", "false" });
        String s = (o == null) ? (String) null : o.toString();
        s = editSymbol(s, c);
        return new Boolean(s);
    }

    private Object editItem(Object originalItem) {
        Object editedItem;
        ValueType type = getCurrentType();

        if (type == ValueType.BOOLEAN) {
            editedItem = editBoolean(originalItem);
        } else if (type == ValueType.CLS) {
            showInstance((Instance) originalItem);
            editedItem = originalItem;
        } else if (type == ValueType.FLOAT) {
            editedItem = editText(originalItem, null);
        } else if (type == ValueType.INSTANCE) {
            showInstance((Instance) originalItem);
            editedItem = originalItem;
        } else if (type == ValueType.INTEGER) {
            editedItem = editText(originalItem, null);
        } else if (type == ValueType.STRING) {
            editedItem = editText(originalItem, null);
        } else if (type == ValueType.SYMBOL) {
            editedItem = editSymbol(originalItem);
        } else {
            Log.error("bad type", this, "editItem", originalItem);
            editedItem = originalItem;
        }
        return editedItem;
    }

    private String editSymbol(Object o) {
        return editSymbol((String) o, getAllowedValues());
    }

    private String editSymbol(String value, Collection allowedValues) {
        return DisplayUtilities.pickSymbol(this, "Value", value, allowedValues);
    }

    private Object editText(Object o, NumberValidator v) {
        String text = DisplayUtilities.editString(this, "Value", o, v);
        Object output;
        if (v == null) {
            output = text;
        } else {
            output = v.convertToNumber(text);
        }
        return output;
    }

    private Action getAddAction() {
        _addAction =
            new AddAction("Add existing value") {
                public void onAdd() {
                    Collection c = addItems();
                    if (c != null) {
                        addItems(c);
                    }
                }
            }
        ;
        return _addAction;
    }

    private Collection getAllowedClses() {
        Collection clses;
        Instance instance = getInstance();
        if (instance instanceof Slot) {
            clses = ((Slot) instance).getAllowedClses();
        } else {
            clses = ((Cls) instance).getTemplateSlotAllowedClses(getSlot());
        }
        return rootCollection(clses);
    }

    private Collection getAllowedParents() {
        Collection parents;
        Instance instance = getInstance();
        if (instance instanceof Slot) {
            parents = ((Slot) instance).getAllowedParents();
        } else {
            parents = ((Cls) instance).getTemplateSlotAllowedParents(getSlot());
        }
        return rootCollection(parents);
    }

    private Collection getAllowedValues() {
        Collection values;
        Instance instance = getInstance();
        if (instance instanceof Slot) {
            values = ((Slot) instance).getAllowedValues();
        } else {
            values = ((Cls) instance).getTemplateSlotAllowedValues(getSlot());
        }
        return values;
    }

    private Action getCreateAction() {
        _createAction =
            new CreateAction("Create a new value") {
                public void onCreate() {
                    Object o = editItem(null);
                    if (o != null) {
                        addItem(o);
                    }
                }
            }
        ;
        return _createAction;
    }

    private ValueType getCurrentType() {
        ValueType type;
        Instance instance = getInstance();
        if (instance == null) {
            type = null;
        } else if (instance instanceof Slot) {
            type = ((Slot) instance).getValueType();
        } else {
            type = ((Cls) instance).getTemplateSlotValueType(getSlot());
        }
        return type;
    }

    private Action getEditAction() {
        _viewAction =
            new ViewAction("View the selected value", this) {
                public void onView(Object o) {
                    Object editedItem = editItem(o);
                    if (!o.equals(editedItem)) {
                        replaceItem(o, editedItem);
                    }
                }
            }
        ;
        return _viewAction;
    }

    private Action getRemoveAction() {
        _removeAction = new RemoveAction("Remove the selected values", this) {
            public void onRemove() {
                removeItems(this.getSelection());
            }
        };
        return _removeAction;
    }

    public void initialize() {
        Action editAction = getEditAction();
        super.initialize(editAction);
        addButton(editAction);
        addButton(getCreateAction());
        addButton(getAddAction());
        addButton(getRemoveAction());
        updateButtons();
    }

    private boolean isEditable() {
        boolean result;
        Slot slot = (Slot) getInstance();
        Cls cls = getAssociatedCls();
        if (cls == null) {
            result = (slot == null) ? true : slot.isEditable();
        } else {
            result = cls.isEditable();
        }
        return result;
    }

    public static boolean isSuitable(Cls cls, Slot slot, Facet facet) {
        return slot.getName().equals(Model.Slot.VALUES);
    }

    private Collection rootCollection(Collection c) {
        return (c.isEmpty()) ? getKnowledgeBase().getRootClses() : c;
    }

    public void setInstance(Instance newInstance) {
        Instance oldInstance = getInstance();
        if (oldInstance != null) {
            if (oldInstance instanceof Slot) {
                oldInstance.removeFrameListener(_frameListener);
            } else {
                ((Cls) oldInstance).removeClsListener(_clsListener);
            }
        }
        super.setInstance(newInstance);
        if (newInstance != null) {
            if (newInstance instanceof Slot) {
                newInstance.addFrameListener(_frameListener);
            } else {
                ((Cls) newInstance).addClsListener(_clsListener);
            }
        }
    }

    public void setValues(Collection c) {
        // Log.enter(this, "setValues", c);
        super.setValues(c);
        updateButtons();
    }

    private void updateButtons() {
        ValueType type = getCurrentType();
        // Log.trace("type=" + type + ", instance=" + getInstance(), this, "updateButtons");
        boolean addAllowed = (type == ValueType.INSTANCE || type == ValueType.CLS);
        _addAction.setAllowed(addAllowed && isEditable());
        boolean createAllowed = !(type == ValueType.ANY || type == ValueType.CLS || type == ValueType.INSTANCE);
        _createAction.setAllowed(createAllowed && isEditable());

        _previousType = type;
    }
}
