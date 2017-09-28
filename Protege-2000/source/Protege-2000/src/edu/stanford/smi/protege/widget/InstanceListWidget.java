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
import edu.stanford.smi.protege.action.*;
import edu.stanford.smi.protege.event.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.util.*;
import edu.stanford.smi.protege.ui.*;

/**
 * Standard widget for acquiring and displaying instances in an ordered list.
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class InstanceListWidget extends AbstractListWidget {
    private AllowableAction _createInstanceAction;
    private AllowableAction _addInstancesAction;
    private AllowableAction _removeInstancesAction;
    private AllowableAction _deleteInstancesAction;
    private AllowableAction _viewInstanceAction;

    private FrameListener _instanceListener = new FrameAdapter() {
        public void browserTextChanged(FrameEvent event) {
            super.browserTextChanged(event);
            // Log.trace("changed", this, "browserTextChanged");
            repaint();
        }
    };

    protected void addButtons(LabeledComponent c) {
        addButton(getViewInstanceAction());
        addButton(getCreateInstanceAction());
        addButton(new ReferencersAction(this), false);
        addButton(getAddInstancesAction());
        addButton(getRemoveInstancesAction());
        addButton(getDeleteInstancesAction(), false);
    }

    public void addItem(Object item) {
        super.addItem(item);
        addListener(CollectionUtilities.createCollection(item));
    }

    public void addItems(Collection items) {
        super.addItems(items);
        addListener(items);
    }

    protected void addListener(Collection values) {
        Iterator i = values.iterator();
        while (i.hasNext()) {
            Instance instance = (Instance) i.next();
            instance.addFrameListener(_instanceListener);
        }
    }

    public void dispose() {
        removeListener(getValues());
        super.dispose();
    }

    protected Action getAddInstancesAction() {
        _addInstancesAction = new AddAction("Select Instances") {
            public void onAdd() {
                handleAddAction();
            }
        };
        return _addInstancesAction;
    }

    public Action getCreateInstanceAction() {
        _createInstanceAction =
            new CreateAction("Create Instance") {
                public void onCreate() {
                    handleCreateAction();
                }
            }
        ;
        return _createInstanceAction;
    }

    protected Action getDeleteInstancesAction() {
        _deleteInstancesAction = new DeleteInstancesAction("Delete Selected Instances", this);
        return _deleteInstancesAction;
    }

    protected Action getRemoveInstancesAction() {
        _removeInstancesAction = new RemoveAction("Remove Selected Instances", this) {
            public void onRemove(Object o) {
                handleRemoveAction((Instance) o);
            }
        };
        return _removeInstancesAction;
    }

    protected Action getViewInstanceAction() {
        _viewInstanceAction = new ViewAction("View Selected Instances", this) {
            public void onView(Object o) {
                handleViewAction((Instance) o);
            }
        };
        return _viewInstanceAction;
    }

    protected void handleAddAction() {
        Collection clses = getCls().getTemplateSlotAllowedClses(getSlot());
        String title = (String) _addInstancesAction.getValue(Action.SHORT_DESCRIPTION);
        addItems(DisplayUtilities.pickInstances(InstanceListWidget.this, clses, title));
    }

    protected void handleCreateAction() {
        Collection clses = getCls().getTemplateSlotAllowedClses(getSlot());
        Cls cls = DisplayUtilities.pickConcreteCls(InstanceListWidget.this, clses);
        if (cls != null) {
            Instance instance = getKnowledgeBase().createInstance(null, cls);
            if (instance instanceof Cls) {
                Cls newcls = (Cls) instance;
                if (newcls.getDirectSuperclassCount() == 0) {
                    newcls.addDirectSuperclass(getKnowledgeBase().getRootCls());
                }
            }
            showInstance(instance);
            addItem(instance);
        }
    }

    protected void handleRemoveAction(Instance instance) {
        removeItem(instance);
    }

    protected void handleViewAction(Instance instance) {
        showInstance((Instance) instance);
    }

    public void initialize() {
        super.initialize();
        addButtons(getLabeledComponent());
        setRenderer(FrameRenderer.createInstance());
    }

    public static boolean isSuitable(Cls cls, Slot slot, Facet facet) {
        boolean isSuitable;
        if (cls == null || slot == null) {
            isSuitable = false;
        } else {
            boolean isInstance = cls.getTemplateSlotValueType(slot) == ValueType.INSTANCE;
            boolean isMultiple = cls.getTemplateSlotAllowsMultipleValues(slot);
            isSuitable = isInstance && isMultiple;
        }
        return isSuitable;
    }

    protected void removeListener(Collection values) {
        Iterator i = values.iterator();
        while (i.hasNext()) {
            Instance instance = (Instance) i.next();
            instance.removeFrameListener(_instanceListener);
        }
    }

    public void setEditable(boolean b) {
        setAllowed(_createInstanceAction, b);
        setAllowed(_addInstancesAction, b);
        setAllowed(_removeInstancesAction, b);
        setAllowed(_deleteInstancesAction, b);
    }

    public void setValues(Collection values) {
        removeListener(getValues());
        addListener(values);
        super.setValues(values);
    }
}
