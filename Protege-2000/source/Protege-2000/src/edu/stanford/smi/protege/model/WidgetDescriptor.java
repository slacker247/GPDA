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

import java.awt.*;
import java.util.*;
import edu.stanford.smi.protege.util.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class WidgetDescriptor implements Cloneable {
    private final static String CLASS_WIDGET = "Widget";
    private final static String SLOT_WIDGET_CLASSNAME = "widget_class_name";
    private final static String SLOT_PROPERTY_LIST = "property_list";
    private final static String SLOT_LABEL = "label";
    private final static String SLOT_NAME = "name";
    private final static String SLOT_HIDDEN = "is_hidden";
    private boolean _isTemporary;
    private boolean _isDirectlyCustomizedByUser;
    private boolean _isIncluded;

    private Instance _instance;

    private WidgetDescriptor(Instance instance) {
        _instance = instance;
    }

    public Object clone() {
        WidgetDescriptor d;
        try {
            d = (WidgetDescriptor) super.clone();
            d._instance = (Instance) _instance.deepCopy(_instance.getKnowledgeBase(), null);
        } catch (CloneNotSupportedException e) {
            Log.exception(e, this, "clone");
            d = null;
        }
        return d;
    }

    public static WidgetDescriptor create(Instance instance) {
        WidgetDescriptor d = null;
        Cls type = instance.getDirectType();
        if (type == null) {
            Log.error("instance has null type", WidgetDescriptor.class, "WidgetDescriptor", instance);
        } else if (type.getName().equals(CLASS_WIDGET)) {
            d = new WidgetDescriptor(instance);
        } else {
            Log.error("not an instance of Widget", WidgetDescriptor.class, "createWidgetDescriptor", instance);
        }
        return d;
    }

    public static WidgetDescriptor create(KnowledgeBase projectKB) {
        Cls widgetCls = projectKB.getCls(CLASS_WIDGET);
        Assert.assertNotNull("widgetCls", widgetCls);
        Instance instance = projectKB.createInstance(null, widgetCls);
        return create(instance);
    }

    private void deletePropertyListInstance(Instance propertyInstance) {
        KnowledgeBase kb = propertyInstance.getKnowledgeBase();
        Collection roots = CollectionUtilities.createCollection(propertyInstance);
        Iterator i = kb.getReachableSimpleInstances(roots).iterator();
        while (i.hasNext()) {
            Instance instance = (Instance) i.next();
            instance.delete();
        }
    }

    public Rectangle getBounds() {
        Rectangle r = InstanceUtilities.getRectangle(_instance);
        if (r.width == 0 || r.height == 0) {
            r = null;
        }
        return r;
    }

    public Instance getInstance() {
        return _instance;
    }

    private Instance getInstanceValue(String slotName) {
        return (Instance) getValue(slotName);
    }

    public String getLabel() {
        String label = getStringValue(SLOT_LABEL);
        /*
        if (label == null) {
            String name = getStringValue(SLOT_NAME);
            if (name != null) {
                label = StringUtilities.symbolToLabel(name);
            }
        }
        */
        return label;
    }

    public Point getLocation() {
        return getBounds().getLocation();
    }

    public String getName() {
        return getStringValue(SLOT_NAME);
    }

    private Instance getOrCreateInstanceValue(String slotName, String className) {
        KnowledgeBase kb = _instance.getKnowledgeBase();
        Instance instance = getInstanceValue(slotName);
        if (instance == null) {
            instance = kb.createInstance(null, kb.getCls(className));
            setValue(slotName, instance);
        }
        Assert.assertEquals("classes", instance.getDirectType().getName(), className);
        return instance;
    }

    public PropertyList getPropertyList() {
        Instance instance = getInstanceValue(SLOT_PROPERTY_LIST);
        if (instance == null) {
            KnowledgeBase kb = _instance.getKnowledgeBase();
            Cls cls = kb.getCls(PropertyList.CLASS_PROPERTY_LIST);
            instance = _instance.getKnowledgeBase().createInstance(null, cls);
            setValue(SLOT_PROPERTY_LIST, instance);
        }
        return new PropertyList(instance);
    }

    private String getStringValue(String slotName) {
        return (String) getValue(slotName);
    }

    private Object getValue(String slotName) {
        return (_instance == null) ? (Object) null : ModelUtilities.getOwnSlotValue(_instance, slotName);
    }

    public String getWidgetClassName() {
        return getStringValue(SLOT_WIDGET_CLASSNAME);
    }

    public boolean isDirectlyCustomizedByUser() {
        return _isDirectlyCustomizedByUser;
    }

    public boolean isIncluded() {
        return _isIncluded;
    }

    public boolean isNull() {
        return getWidgetClassName() == null;
    }

    public boolean isTemporary() {
        return _isTemporary;
    }

    public boolean isVisible() {
        Boolean isHidden = (Boolean) getValue(SLOT_HIDDEN);
        return (isHidden == null) ? true : !isHidden.booleanValue();
    }

    public void setBounds(Rectangle r) {
        InstanceUtilities.setRectangle(_instance, r);
    }

    public void setDirectlyCustomizedByUser(boolean b) {
        if (_isDirectlyCustomizedByUser != b) {
            _isDirectlyCustomizedByUser = b;
            _isTemporary = !_isDirectlyCustomizedByUser;
        }
    }

    public void setIncluded(boolean b) {
        _isIncluded = b;
        setInstancesIncluded(b);
    }

    private void setInstancesIncluded(boolean included) {
        Iterator i = _instance.getReachableSimpleInstances().iterator();
        while (i.hasNext()) {
            Instance instance = (Instance) i.next();
            instance.setIncluded(!included);
        }
    }

    public void setLabel(String label) {
        setValue(SLOT_LABEL, label);
    }

    public void setName(String name) {
        setValue(SLOT_NAME, name);
    }

    public void setPropertyList(PropertyList list) {
        Instance instance = (Instance) getValue(SLOT_PROPERTY_LIST);
        if (instance != null) {
            deletePropertyListInstance(instance);
        }
        // Log.trace(list.getWrappedInstance().getName(), this, "setPropertyList");
        setValue(SLOT_PROPERTY_LIST, list.getWrappedInstance());
    }

    public void setTemporary(boolean b) {
        _isTemporary = b;
    }

    private void setValue(String slotName, Object value) {
        ModelUtilities.setOwnSlotValue(_instance, slotName, value);
    }

    public void setVisible(boolean b) {
        setValue(SLOT_HIDDEN, new Boolean(!b));
    }

    public void setWidgetClassName(String name) {
        String oldName = getWidgetClassName();
        if (!SystemUtilities.equals(name, oldName)) {
            setValue(SLOT_WIDGET_CLASSNAME, name);
            getPropertyList().clear();
        }
    }

    public String toString() {
        return "WidgetDescriptor(" + getWidgetClassName() + ")";
    }
}
