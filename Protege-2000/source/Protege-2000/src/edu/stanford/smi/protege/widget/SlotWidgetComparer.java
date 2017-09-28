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


import java.awt.*;
import java.util.*;
import java.util.List;
import javax.swing.*;
import javax.swing.border.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.widget.*;
import edu.stanford.smi.protege.util.*;

class SlotWidgetComparer implements Comparator {

    private final static int SINGLE = 0;
    private final static int MULTIPLE = 1;
    private final static int _rank[][];

    static {
        _rank = new int[2][8];
        _rank[SINGLE][ValueType.ANY.getIntValue()] = 10;
        _rank[SINGLE][ValueType.BOOLEAN.getIntValue()] = 1;
        _rank[SINGLE][ValueType.CLS.getIntValue()] = 5;
        _rank[SINGLE][ValueType.FLOAT.getIntValue()] = 3;
        _rank[SINGLE][ValueType.INSTANCE.getIntValue()] = 6;
        _rank[SINGLE][ValueType.INTEGER.getIntValue()] = 2;
        _rank[SINGLE][ValueType.STRING.getIntValue()] = 0;
        _rank[SINGLE][ValueType.SYMBOL.getIntValue()] = 4;
        _rank[MULTIPLE][ValueType.ANY.getIntValue()] = 11;
        _rank[MULTIPLE][ValueType.BOOLEAN.getIntValue()] = 7;
        _rank[MULTIPLE][ValueType.CLS.getIntValue()] = 8;
        _rank[MULTIPLE][ValueType.FLOAT.getIntValue()] = 7;
        _rank[MULTIPLE][ValueType.INSTANCE.getIntValue()] = 9;
        _rank[MULTIPLE][ValueType.INTEGER.getIntValue()] = 7;
        _rank[MULTIPLE][ValueType.STRING.getIntValue()] = 7;
        _rank[MULTIPLE][ValueType.SYMBOL.getIntValue()] = 7;
    }

    public int compare(Object o1, Object o2) {
        SlotWidget w1 = (SlotWidget) o1;
        SlotWidget w2 = (SlotWidget) o2;
        int diff = getRank(w1) - getRank(w2);
        if (diff == 0) {
            String s1 = w1.getLabel();
            String s2 = w2.getLabel();
            if (s1 == null || s2 == null) {
                Log.warning("null label", this, "compare", o1, o2);
                diff = 0;
            } else {
                diff = s1.compareToIgnoreCase(s2);
            }
        }
        return diff;
    }

    private static int getRank(SlotWidget widget) {
        Cls cls = widget.getCls();
        Slot slot = widget.getSlot();
        boolean allowsMultiple = cls.getTemplateSlotAllowsMultipleValues(slot);
        ValueType type = cls.getTemplateSlotValueType(slot);
        return _rank[allowsMultiple ? MULTIPLE : SINGLE][type.getIntValue()];
    }
}
