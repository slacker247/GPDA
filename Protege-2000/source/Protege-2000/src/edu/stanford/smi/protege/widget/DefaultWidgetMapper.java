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


import java.lang.reflect.*;
import java.util.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.util.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class DefaultWidgetMapper implements WidgetMapper {
    private final static String METHOD_NAME = "isSuitable";
    private final static Class[] METHOD_ARG_CLASSES = new Class[]{Cls.class, Slot.class, Facet.class};

    private Project _project;
    private KnowledgeBase _knowledgeBase;

    public DefaultWidgetMapper(KnowledgeBase kb, Project project) {
        _project = project;
        _knowledgeBase = kb;
    }

    public WidgetDescriptor createWidgetDescriptor(Cls cls, Slot slot, Facet facet) {
        WidgetDescriptor d = WidgetDescriptor.create(_knowledgeBase);
        String className = getDefaultWidgetClassName(cls, slot, facet);
        if (className == null) {
            d.setWidgetClassName(UglySlotWidget.class.getName());
        } else {
            d.setWidgetClassName(className);
        }
        d.setName(slot.getName());
        return d;
    }

    public String getDefaultWidgetClassName(Cls cls, Slot slot, Facet facet) {
        boolean isMultiple = cls.getTemplateSlotAllowsMultipleValues(slot);
        ValueType type = cls.getTemplateSlotValueType(slot);
        Cls allowedCls = null;
        if (type == ValueType.INSTANCE) {
            Collection allowedClses = cls.getTemplateSlotAllowedClses(slot);
            if (allowedClses.size() == 1) {
                allowedCls = (Cls) CollectionUtilities.getFirstItem(allowedClses);
            }
        }
        return SystemUtilities.getDefaultWidgetClassName(isMultiple, type, allowedCls);
    }

    public Collection getSuitableWidgetClassNames(Cls cls, Slot slot, Facet facet) {
        Collection suitableWidgetClassNames = new ArrayList();
        Iterator i = SystemUtilities.getAvailableSlotWidgetNames().iterator();
        while (i.hasNext()) {
            String className = (String) i.next();
            if (isSuitable(cls, slot, facet, className)) {
                suitableWidgetClassNames.add(className);
            }
        }
        return suitableWidgetClassNames;
    }

    public boolean isSuitable(Cls cls, Slot slot, Facet facet, String className) {
        boolean isSuitable;
        try {
            Class widgetClass = SystemUtilities.forName(className);
            if (widgetClass == null) {
                isSuitable = false;
                Log.warning("Invalid widget class name", this, "isSuitable", cls, slot, facet, className);
            } else {
                Method method = widgetClass.getMethod(METHOD_NAME, METHOD_ARG_CLASSES);
                Boolean result = (Boolean) method.invoke(null, new Object[]{cls, slot, facet});
                isSuitable = result.booleanValue();
            }
        } catch (Exception e) {
            e.printStackTrace();
            isSuitable = false;
        }
        return isSuitable;
    }

    public boolean isSuitableWidget(Cls cls, Slot slot, Facet facet, WidgetDescriptor d) {
        return isSuitable(cls, slot, facet, d.getWidgetClassName());
    }

    private String makeName(String type, boolean isMultiple) {
        String name = "Default ";
        name += type.toString() + " ";
        name += (isMultiple) ? "Multiple" : "Single";
        return name;
    }

    public String toString() {
        return "DefaultWidgetMapper";
    }
}
