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

import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.util.*;
import javax.swing.event.*;
import javax.swing.*;
import java.util.*;
import java.awt.*;

/**
 *  Description of the class
 *
 * @author  Ray Fergerson
 */
public class FloatFieldWidget extends NumberFieldWidget {

    protected String getInvalidTextDescription(String text) {
        String result = null;
        try {
            float f = new Float(text).floatValue();
            Number min = getCls().getTemplateSlotMinimumValue(getSlot());
            if (min != null && f < min.floatValue()) {
                result = "The minimum value is " + min;
            }
            Number max = getCls().getTemplateSlotMaximumValue(getSlot());
            if (max != null && f > max.floatValue()) {
                result = "The maximum value is " + max;
            }
        } catch (NumberFormatException e) {
            result = "The value must be a number";
        }
        return result;
    }

    public Collection getValues() {
        Collection values = new ArrayList();
        String text = getText();
        try {
            values.add(new Float(text));
        } catch (NumberFormatException e) {
            setText(null);
        }
        return values;
    }

    public static boolean isSuitable(Cls cls, Slot slot, Facet facet) {
        boolean result = false;
        if (cls != null && slot != null) {
            boolean isFloat = cls.getTemplateSlotValueType(slot) == ValueType.FLOAT;
            boolean isMultiple = cls.getTemplateSlotAllowsMultipleValues(slot);
            result = isFloat && !isMultiple;
        }
        return result;
    }
}
