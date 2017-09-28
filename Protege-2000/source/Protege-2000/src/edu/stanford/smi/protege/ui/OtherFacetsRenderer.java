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
import javax.swing.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.resource.*;
import edu.stanford.smi.protege.util.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class OtherFacetsRenderer extends DefaultRenderer {

    private void addAllowedClsesText(StringBuffer s, Cls cls, Slot slot) {
        ValueType type = cls.getTemplateSlotValueType(slot);
        if (type == ValueType.INSTANCE) {
            Collection clses = cls.getTemplateSlotAllowedClses(slot);
            addObjects(s, "classes", clses);
        } else if (type == ValueType.CLS) {
            Collection clses = cls.getTemplateSlotAllowedParents(slot);
            addObjects(s, "parents", clses);
        }
    }

    private void addAllowedValuesText(StringBuffer s, Cls cls, Slot slot) {
        if (cls.getTemplateSlotValueType(slot) == ValueType.SYMBOL) {
            Collection values = cls.getTemplateSlotAllowedValues(slot);
            appendValues(s, "allowed-values", values);
        }
    }

    private void addDefaultValuesText(StringBuffer s, Cls cls, Slot slot) {
        Collection defaults = cls.getTemplateSlotDefaultValues(slot);
        if (!defaults.isEmpty()) {
            addObjects(s, "default", defaults);
        }
    }

    private void addInverseSlotText(StringBuffer s, Slot slot) {
        Slot inverseSlot = slot.getInverseSlot();
        if (inverseSlot != null) {
            s.append("inverse-slot=");
            s.append(inverseSlot.getBrowserText());
            s.append(" ");
        }
    }

    private void addObjects(StringBuffer s, String text, Collection objects) {
        Collection strings = new ArrayList();
        Iterator i = objects.iterator();
        while (i.hasNext()) {
            Object o = i.next();
            String name;
            if (o instanceof Frame) {
                name = ((Frame) o).getBrowserText();
            } else {
                name = o.toString();
            }
            strings.add(name);
        }
        appendValues(s, text, strings);
    }

    private void addRangeText(StringBuffer s, Cls cls, Slot slot, ValueType type) {
        Number min = cls.getTemplateSlotMinimumValue(slot);
        Number max = cls.getTemplateSlotMaximumValue(slot);
        if (type == ValueType.INTEGER) {
            if (min != null) {
                min = new Integer(min.intValue());
            }
            if (max != null) {
                max = new Integer(max.intValue());
            }
        }
        if (min != null) {
            s.append("minimum=");
            s.append(min);
        }
        if (min != null && max != null) {
            s.append(", ");
        }
        if (max != null) {
            s.append("maximum=");
            s.append(max);
        }
        if (min != null || max != null) {
            s.append(' ');
        }
    }

    private void addValuesText(StringBuffer s, Cls cls, Slot slot) {
        Collection values = cls.getTemplateSlotValues(slot);
        if (!values.isEmpty()) {
            addObjects(s, "value", values);
        }
    }

    private void appendValues(StringBuffer s, String text, Collection values) {
        boolean first = true;
        s.append(text);
        s.append("={");
        Iterator i = values.iterator();
        while (i.hasNext()) {
            if (first) {
                first = false;
            } else {
                s.append(',');
            }
            s.append(i.next());
        }
        s.append("} ");
    }

    public void load(Object o) {
        FrameSlotCombination combination = (FrameSlotCombination) o;
        Cls cls = (Cls) combination.getFrame();
        Slot slot = combination.getSlot();

        ValueType type = cls.getTemplateSlotValueType(slot);

        StringBuffer text = new StringBuffer();

        if (type == ValueType.INTEGER || type == ValueType.FLOAT) {
            addRangeText(text, cls, slot, type);
        } else if (type == ValueType.SYMBOL) {
            addAllowedValuesText(text, cls, slot);
        } else if (type == ValueType.INSTANCE || type == ValueType.CLS) {
            addAllowedClsesText(text, cls, slot);
        }
        addValuesText(text, cls, slot);
        addDefaultValuesText(text, cls, slot);

        if (!cls.isEditable()) {
            setGrayedText(true);
        }
        setMainText(text.toString());
    }
}
