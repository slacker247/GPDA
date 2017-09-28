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
 * Default implementation of Facet interface.  Forwards all method calls
 * to its DefaultKnowledgeBase.
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class DefaultFacet extends DefaultInstance implements Facet {
    private FacetConstraint _constraint;

    public DefaultFacet(KnowledgeBase kb, FrameID id) {
        super(kb, id);
    }

    public void addFacetListener(FacetListener listener) {
        getDefaultKnowledgeBase().addFacetListener(this, listener);
    }

    public boolean areValidValues(Frame frame, Slot slot, Collection slotValues) {
        boolean result = true;
        if (_constraint != null) {
            Collection facetValues = frame.getOwnSlotFacetValues(slot, this);
            result = _constraint.areValidValues(frame, slot, slotValues, facetValues);
        }
        return result;
    }

    public Slot getAssociatedSlot() {
        return getDefaultKnowledgeBase().getAssociatedSlot(this);
    }

    public FacetConstraint getConstraint() {
        return _constraint;
    }

    public String getInvalidValuesText(Frame frame, Slot slot, Collection slotValues) {
        String result = null;
        if (_constraint != null) {
            Collection facetValues = frame.getOwnSlotFacetValues(slot, this);
            result = _constraint.getInvalidValuesText(frame, slot, slotValues, facetValues);
        }
        return result;
    }

    public String getInvalidValueText(Frame frame, Slot slot, Object item) {
        String result = null;
        if (_constraint != null) {
            Collection facetValues = frame.getOwnSlotFacetValues(slot, this);
            result = _constraint.getInvalidValueText(frame, slot, item, facetValues);
        }
        return result;
    }

    public ValueType getValueType() {
        return getAssociatedSlot().getValueType();
    }

    public boolean isValidValue(Frame frame, Slot slot, Object value) {
        boolean result = true;
        if (_constraint != null) {
            Collection facetValues = frame.getOwnSlotFacetValues(slot, this);
            result = _constraint.isValidValue(frame, slot, value, facetValues);
        }
        return result;
    }

    public void removeFacetListener(FacetListener listener) {
        getDefaultKnowledgeBase().removeFacetListener(this, listener);
    }

    public void setAssociatedSlot(Slot slot) {
        getDefaultKnowledgeBase().setAssociatedSlot(this, slot);
    }

    public void setConstraint(FacetConstraint c) {
        _constraint = c;
    }

    public String toString() {
        StringBuffer buffer = new StringBuffer();
        buffer.append("Facet(");
        buffer.append(getName());
        buffer.append(")");
        return buffer.toString();
    }
}
