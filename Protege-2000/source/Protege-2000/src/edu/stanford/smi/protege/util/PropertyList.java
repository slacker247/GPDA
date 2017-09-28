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

package edu.stanford.smi.protege.util;


import java.awt.Rectangle;
import java.awt.Dimension;
import java.net.*;
import java.util.*;
import java.util.List;
import edu.stanford.smi.protege.event.*;
import edu.stanford.smi.protege.model.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class PropertyList {
    public final static String CLASS_PROPERTY_LIST = "Property_List";
    private final static String CLASS_WIDGET_DESCRIPTOR = "Widget";
    private final static String CLASS_PROPERTY = "Property";
    private final static String CLASS_STRING = "String";
    private final static String CLASS_INTEGER = "Integer";
    private final static String CLASS_BOOLEAN = "Boolean";
    private final static String CLASS_RECTANGLE = "Rectangle";
    private final static String SLOT_NAME = "name";
    private final static String SLOT_BOOLEAN_VALUE = "boolean_value";
    private final static String SLOT_INTEGER_VALUE = "integer_value";
    private final static String SLOT_PROPERTY_VALUE = "property_value";
    private final static String SLOT_STRING_VALUE = "string_value";
    private final static String SLOT_PROPERTIES = "properties";

    private Instance _instance;

    public PropertyList(Instance instance) {
        Assert.assertNotNull("instance", instance);
        Assert.assertEquals("class name", instance.getDirectType().getName(), CLASS_PROPERTY_LIST);
        _instance = instance;
    }

    private void addInstanceValue(String slotName, Instance instance) {
        ModelUtilities.addOwnSlotValue(_instance, slotName, instance);
    }

    private void addProperty(Instance property) {
        Assert.assertNotNull("property", property);
        addInstanceValue(SLOT_PROPERTIES, property);
    }

    public void clear() {
        KnowledgeBase kb = _instance.getKnowledgeBase();
        Collection values = getValues(SLOT_PROPERTIES);
        Iterator i = kb.getReachableSimpleInstances(values).iterator();
        while (i.hasNext()) {
            Instance instance = (Instance) i.next();
            kb.deleteInstance(instance);
        }
        setValue(SLOT_PROPERTIES, null);
    }

    public Object clone() {
        // Instance instance = itsInstance.getKnowledgeBase().createInstance(null, itsInstance.getDirectType());
        Instance instance = (Instance) _instance.deepCopy(null, null);
        return new PropertyList(instance);
    }

    public static PropertyList create(KnowledgeBase kb) {
        Cls cls = kb.getCls(CLASS_PROPERTY_LIST);
        Instance instance = kb.createInstance(null, cls);
        return new PropertyList(instance);
    }

    private Instance createInstance(String clsName) {
        KnowledgeBase kb = _instance.getKnowledgeBase();
        Cls cls = kb.getCls(clsName);
        return kb.createInstance(null, cls);
    }

    private Instance createProperty(String name, String propertyClsName) {
        Instance property = createInstance(propertyClsName);
        setValue(property, SLOT_NAME, name);
        addProperty(property);
        return property;
    }

    public WidgetDescriptor createWidgetDescriptor(String name) {
        Instance instance = getOrCreateProperty(name, CLASS_WIDGET_DESCRIPTOR);
        WidgetDescriptor desc = WidgetDescriptor.create(instance);
        return desc;
    }

    /*
     * private void setProperty(String name, Instance instance) {
     * Instance property = getOrCreateProperty(name);
     * setValue(property, SLOT_PROPERTY_VALUE, instance.getID());
     * }
     */

    public Boolean getBoolean(String name) {
        Instance property = getProperty(name);
        return (property == null) ? (Boolean) null : (Boolean) getValue(property, SLOT_BOOLEAN_VALUE);
    }

    public Dimension getDimension(String name) {
        Instance instance = getProperty(name);
        return (instance == null) ? (Dimension) null : InstanceUtilities.getDimension(instance);
    }

    public Integer getInteger(String name) {
        Instance property = getProperty(name);
        return (property == null) ? (Integer) null : (Integer) getValue(property, SLOT_INTEGER_VALUE);
    }

    public KnowledgeBase getKnowledgeBase() {
        return _instance.getKnowledgeBase();
    }

    public Collection getNames() {
        Collection names = new ArrayList();
        Iterator i = getValues(SLOT_PROPERTIES).iterator();
        while (i.hasNext()) {
            Instance instance = (Instance) i.next();
            names.add(getValue(instance, SLOT_NAME));
        }
        return names;
    }

    private Instance getOrCreateProperty(String propertyName, String propertyClsName) {
        Instance property = getProperty(propertyName);
        if (property == null) {
            property = createProperty(propertyName, propertyClsName);
        } else if (!property.getDirectType().getName().equals(propertyClsName)) {
            property = replaceProperty(propertyName, property, propertyClsName);
        }
        return property;
    }

    private Instance getProperty(String name) {
        KnowledgeBase kb = _instance.getKnowledgeBase();
        Instance property = null;
        Iterator i = getValues(SLOT_PROPERTIES).iterator();
        while (i.hasNext()) {
            Instance instance = (Instance) i.next();
            String propertyName = (String) getValue(instance, SLOT_NAME);
            // Assert.notNull(propertyName, "Instance: " + instance);
            if (name.equals(propertyName)) {
                property = instance;
                break;
            }
        }
        return property;
    }

    public PropertyList getPropertyList(String name) {
        Instance instance = getOrCreateProperty(name, CLASS_PROPERTY_LIST);
        return new PropertyList(instance);
    }

    public Rectangle getRectangle(String name) {
        Instance instance = getProperty(name);
        return (instance == null) ? (Rectangle) null : InstanceUtilities.getRectangle(instance);
    }

    public String getString(String name) {
        Instance property = getProperty(name);
        return (property == null) ? (String) null : (String) getValue(property, SLOT_STRING_VALUE);
    }

    private static Object getValue(Instance instance, String slotName) {
        return ModelUtilities.getOwnSlotValue(instance, slotName);
    }

    private Object getValue(String name) {
        return getValue(_instance, name);
    }

    private static Collection getValues(Instance instance, String slotName) {
        return ModelUtilities.getOwnSlotValues(instance, slotName);
    }

    private Collection getValues(String name) {
        return getValues(_instance, name);
    }

    public WidgetDescriptor getWidgetDescriptor(String name) {
        Instance instance = getProperty(name);
        WidgetDescriptor d;
        if (instance == null) {
            d = null;
        } else {
            d = WidgetDescriptor.create(instance);
        }
        return d;
    }

    public Instance getWrappedInstance() {
        return _instance;
    }

    public void remove(String name) {
        boolean removed = false;
        KnowledgeBase kb = _instance.getKnowledgeBase();
        Assert.assertNotNull("name", name);
        Iterator i = getValues(SLOT_PROPERTIES).iterator();
        while (i.hasNext()) {
            Instance instance = (Instance) i.next();
            if (getValue(instance, SLOT_NAME).equals(name)) {
                removeInstanceValue(SLOT_PROPERTIES, instance);
                removed = true;
                break;
            }
        }
        if (!removed) {
            Log.warning("failed", this, "remove", name);
        }
    }

    private void removeInstanceValue(String slotName, Instance instance) {
        ModelUtilities.removeOwnSlotValue(_instance, slotName, instance);
    }

    private void removeProperty(Instance property) {
        Assert.assertNotNull("property", property);
        removeInstanceValue(SLOT_PROPERTIES, property);
    }

    private void replaceProperty(Instance instance) {
        String name = (String) getValue(instance, SLOT_NAME);
        if (getProperty(name) != null) {
            remove(name);
        }
        addProperty(instance);
    }

    private Instance replaceProperty(String name, Instance property, String propertyClsName) {
        removeProperty(property);
        Instance result = createProperty(name, propertyClsName);
        return result;
    }

    public void setBoolean(String name, Boolean b) {
        setProperty(name, CLASS_BOOLEAN, SLOT_BOOLEAN_VALUE, b);
    }

    public void setBoolean(String name, boolean b) {
        setProperty(name, CLASS_BOOLEAN, SLOT_BOOLEAN_VALUE, new Boolean(b));
    }

    public void setInteger(String name, int i) {
        setProperty(name, CLASS_INTEGER, SLOT_INTEGER_VALUE, new Integer(i));
    }

    public void setInteger(String name, Integer i) {
        setProperty(name, CLASS_INTEGER, SLOT_INTEGER_VALUE, i);
    }

    private void setProperty(String name, String className, String slotName, Object value) {
        Instance property = getOrCreateProperty(name, className);
        setValue(property, slotName, value);
    }

    public void setRectangle(String name, Rectangle r) {
        Instance property = getOrCreateProperty(name, CLASS_RECTANGLE);
        InstanceUtilities.setRectangle(property, r);
    }

    public void setString(String name, String value) {
        setProperty(name, CLASS_STRING, SLOT_STRING_VALUE, value);
    }

    private static void setValue(Instance instance, String slotName, Object value) {
        ModelUtilities.setOwnSlotValue(instance, slotName, value);
    }

    private void setValue(String slotName, Object value) {
        setValue(_instance, slotName, value);
    }

    public void setWidgetDescriptor(WidgetDescriptor d) {
        replaceProperty(d.getInstance());
    }
}
