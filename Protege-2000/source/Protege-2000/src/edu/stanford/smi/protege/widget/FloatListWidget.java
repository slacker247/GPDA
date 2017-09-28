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

import javax.swing.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.ui.*;
import edu.stanford.smi.protege.util.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class FloatListWidget extends AbstractListWidget {

    private Action getCreateAction() {
        return new CreateAction("Add a number to the list") {
            public void onCreate() {
                String s = DisplayUtilities.editString(FloatListWidget.this, "Create Float Value", null, null);
                if (s != null) {
                    addItem(new Float(s));
                }
            }
        };
    }

    private Action getDeleteAction() {
        return new RemoveAction("Delete the selected number", this) {
            public void onRemove() {
                removeItems(this.getSelection());
            }
        };
    }

    private Action getEditAction() {
        return new ViewAction("Edit the selected number", this) {
            public void onView(Object o) {
                String s = DisplayUtilities.editString(FloatListWidget.this, "Edit Float Value", o.toString(), null);
                if (s != null) {
                    replaceItem(o, new Float(s));
                }
            }
        };
    }

    public void initialize() {
        Action editAction = getEditAction();
        super.initialize(editAction);
        addButton(editAction);
        addButton(getCreateAction());
        addButton(getDeleteAction());
    }

    public static boolean isSuitable(Cls cls, Slot slot, Facet facet) {
        boolean isSuitable;
        if (cls == null || slot == null) {
            isSuitable = false;
        } else {
            boolean isFloat = cls.getTemplateSlotValueType(slot) == ValueType.FLOAT;
            boolean isMultiple = cls.getTemplateSlotAllowsMultipleValues(slot);
            isSuitable = isFloat && isMultiple;
        }
        return isSuitable;
    }
}
