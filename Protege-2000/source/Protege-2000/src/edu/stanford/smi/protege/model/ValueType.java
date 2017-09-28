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
 * @author Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class ValueType {
    private static int _nValues;
    private static Map _values = CollectionUtilities.createSmallMap(); // <string, ValueType>
    private String _string;
    private int _intValue;
    private Class _javaType;

    public final static ValueType ANY 		= new ValueType("Any", Object.class);
    public final static ValueType BOOLEAN 	= new ValueType("Boolean", Boolean.class);
    public final static ValueType CLS 		= new ValueType("Class", Cls.class);
    public final static ValueType FLOAT 	= new ValueType("Float", Float.class);
    public final static ValueType INSTANCE 	= new ValueType("Instance", Instance.class);
    public final static ValueType INTEGER 	= new ValueType("Integer", Integer.class);
    public final static ValueType STRING 	= new ValueType("String", String.class);
    public final static ValueType SYMBOL 	= new ValueType("Symbol", String.class);

    private ValueType(String s, Class javaType) {
        _values.put(s, this);
        _string = s;
        _javaType = javaType;
        _intValue = _nValues++;
    }

    public int getIntValue() {
        return _intValue;
    }

    /** @return a unique integer that can be used in "case" statements */
    public Class getJavaType() {
        return _javaType;
    }

    /** @return String representations of all of the allowed value-types */
    public static Collection getValues() {
        return Collections.unmodifiableCollection(_values.values());
    }

    /** @return a String representation of this values-type (e.g. "String") */
    public String toString() {
        return _string;
    }

    /** @param s a String representation of a value-type.  Normally this will have
     *  been obtained from a previous call to "toString()" on a ValueType object.
     *  @return the corresponding value-type
     */
    public static ValueType valueOf(String s) {
        ValueType type = (ValueType) _values.get(s);
        Assert.assertNotNull("type", type);
        return type;
    }
}
