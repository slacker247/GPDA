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
 * Default implementation of Slot interface.  Forwards all method calls
 * to its DefaultKnowledgeBase.
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class DefaultSlot extends DefaultInstance implements Slot {

    public DefaultSlot(KnowledgeBase kb, FrameID id) {
        super(kb, id);
    }

    public void addDirectSuperslot(Slot superslot) {
        getDefaultKnowledgeBase().addDirectSuperslot(this, superslot);
    }

    public void addSlotListener(SlotListener listener) {
        getDefaultKnowledgeBase().addSlotListener(this, listener);
    }

    public Frame deepCopy(KnowledgeBase kb, Map valueMap) {
        Assert.fail("not implemented");
        return this;
    }

    public Collection getAllowedClses() {
        return getDefaultKnowledgeBase().getAllowedClses(this);
    }

    public Collection getAllowedParents() {
        return getDefaultKnowledgeBase().getAllowedParents(this);
    }

    public Collection getAllowedValues() {
        return getDefaultKnowledgeBase().getAllowedValues(this);
    }

    public boolean getAllowsMultipleValues() {
        return getDefaultKnowledgeBase().getAllowsMultipleValues(this);
    }

    public Facet getAssociatedFacet() {
        return getDefaultKnowledgeBase().getAssociatedFacet(this);
    }

    public Collection getDefaultValues() {
        return getDefaultKnowledgeBase().getDefaultValues(this);
    }

    public int getDirectSubslotCount() {
        return getDefaultKnowledgeBase().getDirectSubslotCount(this);
    }

    public Collection getDirectSubslots() {
        return getDefaultKnowledgeBase().getDirectSubslots(this);
    }

    public int getDirectSuperslotCount() {
        return getDefaultKnowledgeBase().getDirectSuperslotCount(this);
    }

    public Collection getDirectSuperslots() {
        return getDefaultKnowledgeBase().getDirectSuperslots(this);
    }

    public Collection getDocumentation() {
        return getDefaultKnowledgeBase().getDocumentation(this);
    }

    public Slot getInverseSlot() {
        return getDefaultKnowledgeBase().getInverseSlot(this);
    }

    public int getMaximumCardinality() {
        return getDefaultKnowledgeBase().getMaximumCardinality(this);
    }

    public Number getMaximumValue() {
        return getDefaultKnowledgeBase().getMaximumValue(this);
    }

    public int getMinimumCardinality() {
        return getDefaultKnowledgeBase().getMinimumCardinality(this);
    }

    public Number getMinimumValue() {
        return getDefaultKnowledgeBase().getMinimumValue(this);
    }

    public Collection getSubslots() {
        return getDefaultKnowledgeBase().getSubslots(this);
    }

    public Collection getSuperslots() {
        return getDefaultKnowledgeBase().getSuperslots(this);
    }

    public Collection getTemplateSlotClses() {
        return getDefaultKnowledgeBase().getTemplateSlotClses(this);
    }

    public Collection getValues() {
        return getDefaultKnowledgeBase().getValues(this);
    }

    public ValueType getValueType() {
        return getDefaultKnowledgeBase().getValueType(this);
    }

    public boolean hasValueAtSomeFrame() {
        return getDefaultKnowledgeBase().hasSlotValueAtSomeFrame(this);
    }

    public void removeDirectSuperslot(Slot superslot) {
        getDefaultKnowledgeBase().removeDirectSuperslot(this, superslot);
    }

    public void removeSlotListener(SlotListener listener) {
        getDefaultKnowledgeBase().removeSlotListener(this, listener);
    }

    public void setAllowedClses(Collection c) {
        getDefaultKnowledgeBase().setAllowedClses(this, c);
    }

    public void setAllowedParents(Collection c) {
        getDefaultKnowledgeBase().setAllowedParents(this, c);
    }

    public void setAllowedValues(Collection c) {
        getDefaultKnowledgeBase().setAllowedValues(this, c);
    }

    public void setAllowsMultipleValues(boolean b) {
        getDefaultKnowledgeBase().setAllowsMultipleValues(this, b);
    }

    public void setAssociatedFacet(Facet facet) {
        getDefaultKnowledgeBase().setAssociatedFacet(this, facet);
    }

    public void setDefaultValues(Collection values) {
        getDefaultKnowledgeBase().setDefaultValues(this, values);
    }

    public void setDirectTypeOfSubslots(Cls cls) {
        getDefaultKnowledgeBase().setDirectTypeOfSubslots(this, cls);
    }

    public void setDocumentation(String doc) {
        Collection docs = CollectionUtilities.createCollection(doc);
        getDefaultKnowledgeBase().setDocumentation(this, docs);
    }

    public void setInverseSlot(Slot slot) {
        getDefaultKnowledgeBase().setInverseSlot(this, slot);
    }

    public void setMaximumCardinality(int max) {
        getDefaultKnowledgeBase().setMaximumCardinality(this, max);
    }

    public void setMaximumValue(Number n) {
        getDefaultKnowledgeBase().setMaximumValue(this, n);
    }

    public void setMinimumCardinality(int min) {
        getDefaultKnowledgeBase().setMinimumCardinality(this, min);
    }

    public void setMinimumValue(Number n) {
        getDefaultKnowledgeBase().setMinimumValue(this, n);
    }

    public void setValues(Collection values) {
        getDefaultKnowledgeBase().setValues(this, values);
    }

    public void setValueType(ValueType type) {
        getDefaultKnowledgeBase().setValueType(this, type);
    }

    public String toString() {
        StringBuffer buffer = new StringBuffer();
        buffer.append("Slot(");
        buffer.append(getName());
        buffer.append(")");
        return buffer.toString();
    }
}
