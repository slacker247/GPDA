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

package edu.stanford.smi.protege.model;


import java.util.*;
import edu.stanford.smi.protege.util.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class ValueTypeConstraint extends AbstractFacetConstraint {
    private static Collection _values;

    static {
        _values = new ArrayList();
        _values.add(ValueType.ANY.toString());
        _values.add(ValueType.BOOLEAN.toString());
        _values.add(ValueType.CLS.toString());
        _values.add(ValueType.FLOAT.toString());
        _values.add(ValueType.INSTANCE.toString());
        _values.add(ValueType.INTEGER.toString());
        _values.add(ValueType.STRING.toString());
        _values.add(ValueType.SYMBOL.toString());
    }

    private static Collection getAllButFirst(Collection c) {
        Collection values;
        if (c == null || c.size() < 1) {
            Log.stack("empty collection", ValueTypeConstraint.class, "getAllButFirst", c);
            values = Collections.EMPTY_LIST;
        } else {
            ArrayList list = new ArrayList(c);
            list.remove(0);
            values = list;
        }
        return values;
    }

    public static Collection getAllowedClses(Collection bindingValue) {
        return getAllButFirst(bindingValue);
    }

    public static Collection getAllowedParents(Collection bindingValue) {
        return getAllButFirst(bindingValue);
    }

    public static Collection getAllowedValues(Collection bindingValue) {
        return getAllButFirst(bindingValue);
    }

    public String getInvalidAnyValueText(Object value) {
        boolean isValid =
            value instanceof Boolean || value instanceof Frame || value instanceof String || value instanceof Number;
        return isValid ? (String) null : "Value must by one of the allowed types";
    }

    public String getInvalidBooleanValueText(Object value) {
        if (value instanceof String) {
            Log.warning("String value in boolean slot", this, "getInvalidBooleanValueText", value);
        }
        return (value instanceof Boolean) ? (String) null : "Value must be a boolean";
    }

    public String getInvalidClsValueText(Object value, Collection allowedParents) {
        String result = null;
        if (value instanceof Cls) {
            Cls cls = (Cls) value;
            if (!allowedParents.contains(cls)) {
                boolean foundParent = false;
                Iterator i = allowedParents.iterator();
                while (!foundParent && i.hasNext()) {
                    Cls parent = (Cls) i.next();
                    if (cls.hasSuperclass(parent)) {
                        foundParent = true;
                    }
                }
                if (!foundParent) {
                    result = "Value must be a subclass of one of the allowed parents";
                }
            }
        } else {
            result = "Value must be a class";
        }
        return result;
    }

    public String getInvalidFloatValueText(Object value) {
        return (value instanceof Float) ? (String) null : "Value must be a floating point number";
    }

    public String getInvalidInstanceValueText(Object value, Collection allowedClses) {
        String result = null;
        if (value instanceof Instance) {
            Instance instance = (Instance) value;
            boolean foundType = false;
            Iterator i = allowedClses.iterator();
            while (!foundType && i.hasNext()) {
                Cls type = (Cls) i.next();
                if (instance.hasType(type)) {
                    foundType = true;
                }
            }
            if (!foundType) {
                result = "Value must be an instance the allowed classes";
            }
        } else {
            result = "Value must be an instance";
        }
        return result;
    }

    public String getInvalidIntegerValueText(Object value) {
        return (value instanceof Integer) ? (String) null : "Value must be an integer";
    }

    public String getInvalidStringValueText(Object value) {
        return (value instanceof String) ? (String) null : "Value must be a string";
    }

    public String getInvalidSymbolValueText(Object value, Collection allowedValues) {
        return allowedValues.contains(value) ? (String) null : "'" + value + "' is not one of the allowed values";
    }

    public String getInvalidValuesText(Frame frame, Slot slot, Collection slotValues, Collection facetValues) {
        String result = null;
        ValueType type = getType(facetValues);
        Iterator i = slotValues.iterator();
        while (result == null && i.hasNext()) {
            Object value = i.next();
            result = getInvalidValueText(facetValues, type, value);
        }
        return result;
    }

    public String getInvalidValueText(Frame frame, Slot slot, Object value, Collection facetValues) {
        ValueType type = getType(facetValues);
        return getInvalidValueText(facetValues, type, value);
    }

    private String getInvalidValueText(Collection facetValues, ValueType type, Object value) {
        String result = null;
        if (type == ValueType.BOOLEAN) {
            result = getInvalidBooleanValueText(value);
        } else if (type == ValueType.CLS) {
            result = getInvalidClsValueText(value, getAllowedParents(facetValues));
        } else if (type == ValueType.FLOAT) {
            result = getInvalidFloatValueText(value);
        } else if (type == ValueType.INSTANCE) {
            result = getInvalidInstanceValueText(value, getAllowedClses(facetValues));
        } else if (type == ValueType.INTEGER) {
            result = getInvalidIntegerValueText(value);
        } else if (type == ValueType.STRING) {
            result = getInvalidStringValueText(value);
        } else if (type == ValueType.SYMBOL) {
            result = getInvalidSymbolValueText(value, getAllowedValues(facetValues));
        } else if (type == ValueType.ANY) {
            result = getInvalidAnyValueText(value);
        } else {
            Assert.fail("Invalid type: " + type);
            result = "failed";
        }
        return result;
    }

    public static ValueType getType(Collection bindingValue) {
        Object first = CollectionUtilities.getFirstItem(bindingValue);
        ValueType type;
        if (first == null) {
            // Log.warning("empty collection", ValueTypeConstraint.class, "getType", bindingValue);
            type = ValueType.ANY;
        } else {
            type = (ValueType) ValueType.valueOf((String) first);
        }
        return type;
    }

    public static Collection getValues() {
        return _values;
    }

    public static Collection getValues(ValueType type) {
        return getValues(type, Collections.EMPTY_LIST);
    }

    public static List getValues(ValueType type, Collection values) {
        List value = new ArrayList();
        value.add(type.toString());
        if (values != null) {
            value.addAll(values);
        }
        return value;
    }
}
