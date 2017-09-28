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

/**
 * A frame which has one or more superclasses, may have subclasses, and which exhibits
 * inheritance behavior (such as inheritance of slots). The specific inheritance behavior
 * (such as propagation of slots and facets) are left unspecified by the
 * interface.
 *
 * See #Frame for a discussion of "own" and "template" slots.
 *
 * @author Ray Fergerson <fergerson@smi.stanford.edu>
 */
public interface Cls extends Instance {

    void addClsListener(ClsListener listener);

    void addDirectSuperclass(Cls cls);

    void addDirectTemplateSlot(Slot slot);

    void addTemplateFacetValue(Slot slot, Facet facet, Object value);

    void addTemplateSlotValue(Slot slot, Object value);

    Instance createDirectInstance(String name);

    Slot getBrowserSlot();

    /**
     * @deprecated
     */
    Collection getConcreteSubclasses();

    Slot getDirectBrowserSlot();

    int getDirectInstanceCount();

    Collection getDirectInstances();

    int getDirectSubclassCount();

    Collection getDirectSubclasses();

    int getDirectSuperclassCount();

    Collection getDirectSuperclasses();

    Collection getDirectTemplateFacetValues(Slot slot, Facet facet);

    Collection getDirectTemplateSlots();

    Collection getDirectTemplateSlotValues(Slot slot);

    int getInstanceCount();

    Collection getInstances();

    Collection getSubclasses();

    Collection getSuperclasses();

    Collection getTemplateFacets(Slot slot);

    /** see {@link Frame} for a description of the return type */
    Object getTemplateFacetValue(Slot slot, Facet facet);

    /** see {@link Frame} for a description of the return type */
    Collection getTemplateFacetValues(Slot slot, Facet facet);

    Collection getTemplateSlotAllowedClses(Slot slot);

    Collection getTemplateSlotAllowedParents(Slot slot);

    Collection getTemplateSlotAllowedValues(Slot slot);

    boolean getTemplateSlotAllowsMultipleValues(Slot slot);

    /** see {@link Frame} for a description of the return type */
    Collection getTemplateSlotDefaultValues(Slot slot);

    Collection getTemplateSlotDocumentation(Slot slot);

    /** returns 0 if there is no maximum cardinality */
    int getTemplateSlotMaximumCardinality(Slot slot);

    Number getTemplateSlotMaximumValue(Slot slot);

    int getTemplateSlotMinimumCardinality(Slot slot);

    Number getTemplateSlotMinimumValue(Slot slot);

    Collection getTemplateSlots();

    /** see {@link Frame} for a description of the return type */
    Object getTemplateSlotValue(Slot slot);

    /** see {@link Frame} for a description of the return type */
    Collection getTemplateSlotValues(Slot slot);

    ValueType getTemplateSlotValueType(Slot slot);

    int getVisibleDirectSubclassCount();

    Collection getVisibleDirectSubclasses();

    /**
     * returns true if the facet is overridden on this slot relative to the value
     * at this classes direct parent.
     */
    boolean hasDirectlyOverriddenTemplateFacet(Slot slot, Facet facet);

    /**
     * returns true if any facet of this slot at this class is
     * directly overridden.  "directly overridden" means that the slot is
     * overridden at this class relative to the direct parent class or classes.
     */
    boolean hasDirectlyOverriddenTemplateSlot(Slot slot);

    boolean hasDirectSuperclass(Cls cls);

    boolean hasDirectTemplateSlot(Slot slot);

    boolean hasInheritedTemplateSlot(Slot slot);

    /**
     * returns true if the facet is overridden on this slot relative to the value of
     * the facets associated slot on the top-level slot.
     */
    boolean hasOverriddenTemplateFacet(Slot slot, Facet facet);

    /**
     * returns true if any facet of this slot at this class is
     * overridden.  "overridden" refers to an override relative to the
     * top-level slot.
     */
    boolean hasOverriddenTemplateSlot(Slot slot);

    boolean hasSuperclass(Cls cls);

    boolean hasTemplateFacet(Slot slot, Facet facet);

    boolean hasTemplateSlot(Slot slot);

    boolean isAbstract();

    boolean isClsMetaCls();

    boolean isConcrete();

    boolean isDefaultClsMetaCls();

    boolean isDefaultFacetMetaCls();

    boolean isDefaultSlotMetaCls();

    boolean isFacetMetaCls();

    boolean isMetaCls();

    boolean isRoot();

    boolean isSlotMetaCls();

    /**
     * Reorder the subclasses, moving <code>movedSubclass</code> so that it appears
     * after <code>afterCls</code> .  If aftetCls is null then the movedSubclass appears
     * first
     */
    void moveDirectSubclass(Cls movedSubclass, Cls afterCls);

    void removeClsListener(ClsListener listener);

    void removeDirectSuperclass(Cls cls);

    void removeDirectTemplateSlot(Slot slot);

    void removeTemplateFacetOverrides(Slot slot);

    void setAbstract(boolean v);

    void setDirectBrowserSlot(Slot slot);

    void setDirectTypeOfSubclasses(Cls metaCls);

    /** see {@link Frame} for a description of the value type */
    void setTemplateFacetValue(Slot slot, Facet facet, Object value);

    /** see {@link Frame} for a description of the value type */
    void setTemplateFacetValues(Slot slot, Facet facet, Collection c);

    void setTemplateSlotAllowedClses(Slot slot, Collection clses);

    void setTemplateSlotAllowedParents(Slot slot, Collection clses);

    void setTemplateSlotAllowedValues(Slot slot, Collection values);

    void setTemplateSlotAllowsMultipleValues(Slot slot, boolean b);

    /** see {@link Frame} for a description of the value type */
    void setTemplateSlotDefaultValues(Slot slot, Collection values);

    void setTemplateSlotDocumentation(Slot slot, String documentation);

    void setTemplateSlotDocumentation(Slot slot, Collection documentation);

    /** sets the maximum cardinality.  max = 0 => no maximum */
    void setTemplateSlotMaximumCardinality(Slot slot, int max);

    /** sets the maximum slot value.  max = null => no maximum */
    void setTemplateSlotMaximumValue(Slot slot, Number max);

    /** sets the minimum cardinality */
    void setTemplateSlotMinimumCardinality(Slot slot, int min);

    void setTemplateSlotMinimumValue(Slot slot, Number min);

    /** see {@link Frame} for a description of the value type */
    void setTemplateSlotValue(Slot slot, Object value);

    /** see {@link Frame} for a description of the value type */
    void setTemplateSlotValues(Slot slot, Collection c);

    void setTemplateSlotValueType(Slot slot, ValueType valueType);
}
