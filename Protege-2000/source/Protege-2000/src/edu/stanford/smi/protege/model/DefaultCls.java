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
import edu.stanford.smi.protege.event.*;
import edu.stanford.smi.protege.util.*;

/**
 * Default implementation of Cls interface.  Forwards all method calls
 * to its DefaultKnowledgeBase.
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class DefaultCls extends DefaultInstance implements Cls {

    public DefaultCls(KnowledgeBase kb, FrameID id) {
        super(kb, id);
    }

    public void addClsListener(ClsListener listener) {
        getDefaultKnowledgeBase().addClsListener(this, listener);
    }

    public void addDirectSuperclass(Cls superclass) {
        getDefaultKnowledgeBase().addDirectSuperclass(this, superclass);
    }

    public void addDirectTemplateSlot(Slot slot) {
        getDefaultKnowledgeBase().addDirectTemplateSlot(this, slot);
    }

    public void addTemplateFacetValue(Slot slot, Facet facet, Object value) {
        getDefaultKnowledgeBase().addTemplateFacetValue(this, slot, facet, value);
    }

    public void addTemplateSlotValue(Slot slot, Object value) {
        getDefaultKnowledgeBase().addTemplateSlotValue(this, slot, value);
    }

    public Instance createDirectInstance(String name) {
        return getDefaultKnowledgeBase().createInstance(name, this);
    }

    public Slot getBrowserSlot() {
        Slot browserSlot;
        Project p = getProject();
        if (p == null) {
            browserSlot = getDefaultKnowledgeBase().getNameSlot();
        } else {
            browserSlot = getProject().getBrowserSlot(this);
        }
        return browserSlot;
    }

    /**
     *
     *
     * @deprecated
     * @return        The ConcreteSubclasses value
     */
    public Collection getConcreteSubclasses() {
        Collection subclasses = new ArrayList(getSubclasses());
        Iterator i = subclasses.iterator();
        while (i.hasNext()) {
            Cls cls = (Cls) i.next();
            if (cls.isAbstract()) {
                i.remove();
            }
        }
        return subclasses;
    }

    public Slot getDirectBrowserSlot() {
        return getProject().getDirectBrowserSlot(this);
    }

    public int getDirectInstanceCount() {
        return getDefaultKnowledgeBase().getDirectInstanceCount(this);
    }

    public Collection getDirectInstances() {
        return getDefaultKnowledgeBase().getDirectInstances(this);
    }

    public int getDirectSubclassCount() {
        return getDefaultKnowledgeBase().getDirectSubclassCount(this);
    }

    public Collection getDirectSubclasses() {
        return getDefaultKnowledgeBase().getDirectSubclasses(this);
    }

    public int getDirectSuperclassCount() {
        return getDefaultKnowledgeBase().getDirectSuperclassCount(this);
    }

    public Collection getDirectSuperclasses() {
        return getDefaultKnowledgeBase().getDirectSuperclasses(this);
    }

    public Collection getDirectTemplateFacetValues(Slot slot, Facet facet) {
        return getDefaultKnowledgeBase().getDirectTemplateFacetValues(this, slot, facet);
    }

    public Collection getDirectTemplateSlots() {
        return getDefaultKnowledgeBase().getDirectTemplateSlots(this);
    }

    public Collection getDirectTemplateSlotValues(Slot slot) {
        return getDefaultKnowledgeBase().getDirectTemplateSlotValues(this, slot);
    }

    public int getInstanceCount() {
        return getDefaultKnowledgeBase().getInstanceCount(this);
    }

    public Collection getInstances() {
        return getDefaultKnowledgeBase().getInstances(this);
    }

    public Collection getSubclasses() {
        return getDefaultKnowledgeBase().getSubclasses(this);
    }

    public Collection getSuperclasses() {
        return getDefaultKnowledgeBase().getSuperclasses(this);
    }

    public Collection getTemplateFacets(Slot slot) {
        return getDefaultKnowledgeBase().getTemplateFacets(this, slot);
    }

    public Object getTemplateFacetValue(Slot slot, Facet facet) {
        return getDefaultKnowledgeBase().getTemplateFacetValue(this, slot, facet);
    }

    public Collection getTemplateFacetValues(Slot slot, Facet facet) {
        return getDefaultKnowledgeBase().getTemplateFacetValues(this, slot, facet);
    }

    public Collection getTemplateSlotAllowedClses(Slot slot) {
        return getDefaultKnowledgeBase().getTemplateSlotAllowedClses(this, slot);
    }

    public Collection getTemplateSlotAllowedParents(Slot slot) {
        return getDefaultKnowledgeBase().getTemplateSlotAllowedParents(this, slot);
    }

    public Collection getTemplateSlotAllowedValues(Slot slot) {
        return getDefaultKnowledgeBase().getTemplateSlotAllowedValues(this, slot);
    }

    public boolean getTemplateSlotAllowsMultipleValues(Slot slot) {
        return getDefaultKnowledgeBase().getTemplateSlotAllowsMultipleValues(this, slot);
    }

    public Collection getTemplateSlotDefaultValues(Slot slot) {
        return getDefaultKnowledgeBase().getTemplateSlotDefaultValues(this, slot);
    }

    public Collection getTemplateSlotDocumentation(Slot slot) {
        return getDefaultKnowledgeBase().getTemplateSlotDocumentation(this, slot);
    }

    public int getTemplateSlotMaximumCardinality(Slot slot) {
        return getDefaultKnowledgeBase().getTemplateSlotMaximumCardinality(this, slot);
    }

    public Number getTemplateSlotMaximumValue(Slot slot) {
        return getDefaultKnowledgeBase().getTemplateSlotMaximumValue(this, slot);
    }

    public int getTemplateSlotMinimumCardinality(Slot slot) {
        return getDefaultKnowledgeBase().getTemplateSlotMinimumCardinality(this, slot);
    }

    public Number getTemplateSlotMinimumValue(Slot slot) {
        return getDefaultKnowledgeBase().getTemplateSlotMinimumValue(this, slot);
    }

    public Collection getTemplateSlots() {
        return getDefaultKnowledgeBase().getTemplateSlots(this);
    }

    public Object getTemplateSlotValue(Slot slot) {
        return getDefaultKnowledgeBase().getTemplateSlotValue(this, slot);
    }

    public Collection getTemplateSlotValues(Slot slot) {
        return getDefaultKnowledgeBase().getTemplateSlotValues(this, slot);
    }

    public ValueType getTemplateSlotValueType(Slot slot) {
        return getDefaultKnowledgeBase().getTemplateSlotValueType(this, slot);
    }

    public int getVisibleDirectSubclassCount() {
        return getVisibleDirectSubclasses().size();
    }

    public Collection getVisibleDirectSubclasses() {
        Collection subclasses = new ArrayList(getDirectSubclasses());
        Iterator i = subclasses.iterator();
        while (i.hasNext()) {
            Object o = i.next();
            if (!(o instanceof Cls)) {
                Log.error("subclass is not class: " + o, this, "getVisibleDirectSubclasses");
                i.remove();
                continue;
            }
            Cls subclass = (Cls) o;
            if (!subclass.isVisible()) {
                i.remove();
            }
        }
        return subclasses;
    }

    public boolean hasDirectlyOverriddenTemplateFacet(Slot slot, Facet facet) {
        return getDefaultKnowledgeBase().hasDirectlyOverriddenTemplateFacet(this, slot, facet);
    }

    public boolean hasDirectlyOverriddenTemplateSlot(Slot slot) {
        return getDefaultKnowledgeBase().hasDirectlyOverriddenTemplateSlot(this, slot);
    }

    public boolean hasDirectSuperclass(Cls cls) {
        return getDefaultKnowledgeBase().hasDirectSuperclass(this, cls);
    }

    public boolean hasDirectTemplateSlot(Slot slot) {
        return getDefaultKnowledgeBase().hasDirectTemplateSlot(this, slot);
    }

    public boolean hasInheritedTemplateSlot(Slot slot) {
        return getDefaultKnowledgeBase().hasInheritedTemplateSlot(this, slot);
    }

    public boolean hasOverriddenTemplateFacet(Slot slot, Facet facet) {
        return getDefaultKnowledgeBase().hasOverriddenTemplateFacet(this, slot, facet);
    }

    public boolean hasOverriddenTemplateSlot(Slot slot) {
        return getDefaultKnowledgeBase().hasOverriddenTemplateSlot(this, slot);
    }

    public boolean hasSuperclass(Cls cls) {
        return getDefaultKnowledgeBase().hasSuperclass(this, cls);
    }

    public boolean hasTemplateFacet(Slot slot, Facet facet) {
        return getTemplateFacets(slot).contains(facet);
    }

    public boolean hasTemplateSlot(Slot slot) {
        return getDefaultKnowledgeBase().hasTemplateSlot(this, slot);
    }

    public boolean isAbstract() {
        return getDefaultKnowledgeBase().isAbstract(this);
    }

    public boolean isClsMetaCls() {
        return getDefaultKnowledgeBase().isClsMetaCls(this);
    }

    public boolean isConcrete() {
        return !isAbstract();
    }

    public boolean isDefaultClsMetaCls() {
        return getDefaultKnowledgeBase().isDefaultClsMetaCls(this);
    }

    public boolean isDefaultFacetMetaCls() {
        return getDefaultKnowledgeBase().isDefaultFacetMetaCls(this);
    }

    public boolean isDefaultSlotMetaCls() {
        return getDefaultKnowledgeBase().isDefaultSlotMetaCls(this);
    }

    public boolean isFacetMetaCls() {
        return getDefaultKnowledgeBase().isFacetMetaCls(this);
    }

    public boolean isMetaCls() {
        return getDefaultKnowledgeBase().isMetaCls(this);
    }

    public boolean isRoot() {
        return getDefaultKnowledgeBase().getRootCls() == this || getDirectType() == this;
    }

    public boolean isSlotMetaCls() {
        return getDefaultKnowledgeBase().isSlotMetaCls(this);
    }

    public void moveDirectSubclass(Cls movedSubclass, Cls afterCls) {
        getDefaultKnowledgeBase().moveDirectSubclass(this, movedSubclass, afterCls);
    }

    public void removeClsListener(ClsListener listener) {
        getDefaultKnowledgeBase().removeClsListener(this, listener);
    }

    public void removeDirectSuperclass(Cls superclass) {
        getDefaultKnowledgeBase().removeDirectSuperclass(this, superclass);
    }

    public void removeDirectTemplateSlot(Slot slot) {
        getDefaultKnowledgeBase().removeDirectTemplateSlot(this, slot);
    }

    public void removeTemplateFacetOverrides(Slot slot) {
        getDefaultKnowledgeBase().removeTemplateFacetOverrides(this, slot);
    }

    public void setAbstract(boolean isAbstract) {
        getDefaultKnowledgeBase().setAbstract(this, isAbstract);
    }

    public void setDirectBrowserSlot(Slot slot) {
        getProject().setDirectBrowserSlot(this, slot);
    }

    public void setDirectTypeOfSubclasses(Cls metaCls) {
        getDefaultKnowledgeBase().setDirectTypeOfSubclasses(this, metaCls);
    }

    public void setTemplateFacetValue(Slot slot, Facet facet, Object value) {
        getDefaultKnowledgeBase().setTemplateFacetValue(this, slot, facet, value);
    }

    public void setTemplateFacetValues(Slot slot, Facet facet, Collection values) {
        getDefaultKnowledgeBase().setTemplateFacetValues(this, slot, facet, values);
    }

    public void setTemplateSlotAllowedClses(Slot slot, Collection clses) {
        getDefaultKnowledgeBase().setTemplateSlotAllowedClses(this, slot, clses);
    }

    public void setTemplateSlotAllowedParents(Slot slot, Collection parents) {
        getDefaultKnowledgeBase().setTemplateSlotAllowedParents(this, slot, parents);
    }

    public void setTemplateSlotAllowedValues(Slot slot, Collection values) {
        getDefaultKnowledgeBase().setTemplateSlotAllowedValues(this, slot, values);
    }

    public void setTemplateSlotAllowsMultipleValues(Slot slot, boolean multiple) {
        getDefaultKnowledgeBase().setTemplateSlotAllowsMultipleValues(this, slot, multiple);
    }

    public void setTemplateSlotDefaultValues(Slot slot, Collection values) {
        getDefaultKnowledgeBase().setTemplateSlotDefaultValues(this, slot, values);
    }

    public void setTemplateSlotDocumentation(Slot slot, String value) {
        getDefaultKnowledgeBase().setTemplateSlotDocumentation(this, slot, value);
    }

    public void setTemplateSlotDocumentation(Slot slot, Collection values) {
        getDefaultKnowledgeBase().setTemplateSlotDocumentation(this, slot, values);
    }

    public void setTemplateSlotMaximumCardinality(Slot slot, int max) {
        getDefaultKnowledgeBase().setTemplateSlotMaximumCardinality(this, slot, max);
    }

    public void setTemplateSlotMaximumValue(Slot slot, Number maximum) {
        getDefaultKnowledgeBase().setTemplateSlotMaximumValue(this, slot, maximum);
    }

    public void setTemplateSlotMinimumCardinality(Slot slot, int min) {
        getDefaultKnowledgeBase().setTemplateSlotMinimumCardinality(this, slot, min);
    }

    public void setTemplateSlotMinimumValue(Slot slot, Number minimum) {
        getDefaultKnowledgeBase().setTemplateSlotMinimumValue(this, slot, minimum);
    }

    public void setTemplateSlotValue(Slot slot, Object value) {
        getDefaultKnowledgeBase().setTemplateSlotValue(this, slot, value);
    }

    public void setTemplateSlotValues(Slot slot, Collection values) {
        getDefaultKnowledgeBase().setTemplateSlotValues(this, slot, values);
    }

    public void setTemplateSlotValueType(Slot slot, ValueType type) {
        getDefaultKnowledgeBase().setTemplateSlotValueType(this, slot, type);
    }

    public String toString() {
        StringBuffer buffer = new StringBuffer();
        buffer.append("Cls(");
        buffer.append(getName());
        buffer.append(")");
        return buffer.toString();
    }
}
