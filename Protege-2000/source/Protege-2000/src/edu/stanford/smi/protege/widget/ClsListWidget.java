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
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class ClsListWidget extends AbstractListWidget {
    private AllowableAction _addAction;
    private AllowableAction _removeAction;

    private FrameListener _instanceListener =
        new FrameAdapter() {
            public void ownSlotValueChanged(FrameEvent event) {
                repaint();
            }

            public void nameChanged(FrameEvent event) {
                repaint();
            }
        }
    ;

    protected void addButtons(LabeledComponent c, Action viewAction) {
        addButton(viewAction);
        addButton(getAddClsesAction());
        addButton(getRemoveClsesAction());
    }

    public void dispose() {
        super.dispose();
        removeListener();
    }

    protected Action getAddClsesAction() {
        _addAction = new AddAction("Select Classes") {
            public void onAdd() {
                handleAddAction();
            }
        };
        return _addAction;
    }

    protected Action getRemoveClsesAction() {
        _removeAction = new RemoveAction("Remove Selected Classes", this) {
            public void onRemove(Object o) {
                handleRemoveAction((Cls) o);
            }
        };
        return _removeAction;
    }

    protected Action getViewInstanceAction() {
        return new ViewAction("View Selected Classes", this) {
            public void onView(Object o) {
                handleViewAction((Cls) o);
            }
        };
    }

    protected void handleAddAction() {
        Collection clses = getCls().getTemplateSlotAllowedParents(getSlot());
        addItems(DisplayUtilities.pickClses(ClsListWidget.this, clses));
    }

    protected void handleRemoveAction(Cls cls) {
        removeItem(cls);
    }

    protected void handleViewAction(Cls cls) {
        showInstance(cls);
    }

    public void initialize() {
        super.initialize();
        addButtons(getLabeledComponent(), getViewInstanceAction());
        setRenderer(FrameRenderer.createInstance());
    }

    public static boolean isSuitable(Cls cls, Slot slot, Facet facet) {
        boolean isSuitable;
        if (cls == null || slot == null) {
            isSuitable = false;
        } else {
            boolean isCls = cls.getTemplateSlotValueType(slot) == ValueType.CLS;
            boolean isMultiple = cls.getTemplateSlotAllowsMultipleValues(slot);
            isSuitable = isCls && isMultiple;
        }
        return isSuitable;
    }

    protected void removeListener() {
        Iterator i = getValues().iterator();
        while (i.hasNext()) {
            Instance instance = (Instance) i.next();
            instance.removeFrameListener(this._instanceListener);
        }
    }

    public void setEditable(boolean b) {
        setAllowed(_addAction, b);
        setAllowed(_removeAction, b);
    }

    public void setValues(Collection values) {
        removeListener();
        Iterator i = values.iterator();
        while (i.hasNext()) {
            Instance instance = (Instance) i.next();
            instance.addFrameListener(this._instanceListener);
        }
        super.setValues(values);
    }
}
