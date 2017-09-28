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
 * Default implementation of Frame interface.  Forwards all method calls
 * to its DefaultKnowledgeBase.
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public abstract class DefaultFrame implements Frame {
    private boolean _isEditable = true;
    private boolean _isIncluded = false;
    private boolean _isSystem = false;
    private FrameID _frameID;
    private DefaultKnowledgeBase _knowledgeBase;

    private final static char SPECIAL_NAME_CHAR = ':';

    DefaultFrame(KnowledgeBase kb, FrameID frameID) {
        Assert.assertNotNull("knowledge base", kb);
        _knowledgeBase = (DefaultKnowledgeBase) kb;
        if (frameID == null) {
            frameID = _knowledgeBase.allocateFrameID();
        }
        this._frameID = frameID;
    }

    public void addFrameListener(FrameListener listener) {
        getDefaultKnowledgeBase().addFrameListener(this, listener);
    }

    public boolean addOwnFacetValue(Slot slot, Facet facet, Object value) {
        Assert.fail("not implemented");
        return false;
    }

    public void addOwnSlotValue(Slot slot, Object value) {
        getDefaultKnowledgeBase().addOwnSlotValue(this, slot, value);
    }

    public boolean areValidOwnSlotValues(Slot slot, Collection c) {
        return getDefaultKnowledgeBase().areValidOwnSlotValues(this, slot, c);
    }

    private static int compareStrings(String s1, String s2) {
        int result = s1.compareToIgnoreCase(s2);
        if (result == 0) {
            result = s1.compareTo(s2);
        }
        return result;
    }

    public int compareTo(Object o) {
        int result;
        if (o instanceof Frame) {
            Frame f2 = (Frame) o;
            String t1 = this.getBrowserText();
            String t2 = f2.getBrowserText();
            if (t1.charAt(0) == SPECIAL_NAME_CHAR) {
                result = (t2.charAt(0) == SPECIAL_NAME_CHAR) ? compareStrings(t1, t2) : +1;
            } else if (t2.charAt(0) == SPECIAL_NAME_CHAR) {
                result = -1;
            } else {
                result = compareStrings(t1, t2);
            }
        } else {
            result = 0;
        }
        return result;
    }

    private void copyFrameBindingValues(Collection values, Frame copyFrame, Slot copySlot, Map valueMap) {
        KnowledgeBase origKB = getKnowledgeBase();
        KnowledgeBase copyKB = copyFrame.getKnowledgeBase();
        Collection copyValues = new ArrayList();
        Iterator i = values.iterator();
        while (i.hasNext()) {
            Frame origFrame = (Frame) i.next();
            Frame copyValue = (Frame) valueMap.get(origFrame);
            if (copyValue == null) {
                copyValue = origFrame.deepCopy(copyKB, valueMap);
            }
            copyValues.add(copyValue);
        }
        copyFrame.setOwnSlotValues(copySlot, copyValues);
    }

    private void copyOwnSlot(DefaultFrame copyFrame, Slot origSlot, Map valueMap) {
        Slot copySlot = (Slot) valueMap.get(origSlot);
        Assert.assertNotNull("copy slot", copySlot);
        copyOwnSlotValues(copyFrame, copySlot, origSlot, valueMap);
    }

    private void copyOwnSlots(DefaultFrame copyFrame, Map valueMap) {
        Iterator i = getDefaultKnowledgeBase().getOwnSlots(this).iterator();
        while (i.hasNext()) {
            Slot origSlot = (Slot) i.next();
            if (!origSlot.isSystem()) {
                copyOwnSlot(copyFrame, origSlot, valueMap);
            }
        }
    }

    private void copyOwnSlotValues(Frame copyFrame, Slot copySlot, Slot origSlot, Map valueMap) {
        ValueType type = getOwnSlotValueType(origSlot);
        Collection origValues = getOwnSlotValues(origSlot);
        if (type == ValueType.INSTANCE || type == ValueType.CLS) {
            copyFrameBindingValues(origValues, copyFrame, copySlot, valueMap);
        } else {
            copyFrame.setOwnSlotValues(copySlot, origValues);
        }
    }

    public Frame deepCopy(KnowledgeBase kb, Map valueMap) {
        Assert.assertNotNull("knowledge base", kb);
        Assert.assertNotNull("value map", valueMap);
        DefaultFrame copy = (DefaultFrame) valueMap.get(this);
        Assert.assertNotNull(null, copy);
        copyOwnSlots(copy, valueMap);
        return copy;
    }

    public void delete() {
        getDefaultKnowledgeBase().deleteFrame(this);
    }

    public void finalize() {
        try {
            super.finalize();
            // System.out.println(getClass().getName() + " finalize");
        } catch (Throwable t) {
            t.printStackTrace();
        }
    }

    public String getBrowserText() {
        return getName();
    }

    protected DefaultKnowledgeBase getDefaultKnowledgeBase() {
        return _knowledgeBase;
    }

    public Collection getDocumentation() {
        return getDefaultKnowledgeBase().getDocumentation(this);
    }

    public final FrameID getFrameID() {
        return _frameID;
    }

    public String getInvalidOwnSlotValuesText(Slot slot, Collection c) {
        return getDefaultKnowledgeBase().getInvalidOwnSlotValuesText(this, slot, c);
    }

    public String getInvalidOwnSlotValueText(Slot slot, Object o) {
        return getDefaultKnowledgeBase().getInvalidOwnSlotValueText(this, slot, o);
    }

    public final KnowledgeBase getKnowledgeBase() {
        return _knowledgeBase;
    }

    public String getName() {
        return getDefaultKnowledgeBase().getName(this);
    }

    public boolean getOwnSlotAllowsMultipleValues(Slot slot) {
        return getDefaultKnowledgeBase().getOwnSlotAllowsMultipleValues(this, slot);
    }

    public Collection getOwnSlotAndSubslotValues(Slot slot) {
        return getDefaultKnowledgeBase().getOwnSlotAndSubslotValues(this, slot);
    }

    public Collection getOwnSlotDefaultValues(Slot slot) {
        return getDefaultKnowledgeBase().getOwnSlotDefaultValues(this, slot);
    }

    public Collection getOwnSlotFacets(Slot slot) {
        return getDefaultKnowledgeBase().getOwnSlotFacets(this, slot);
    }

    public Collection getOwnSlotFacetValues(Slot slot, Facet facet) {
        return getDefaultKnowledgeBase().getOwnSlotFacetValues(this, slot, facet);
    }

    public Collection getOwnSlots() {
        return getDefaultKnowledgeBase().getOwnSlots(this);
    }

    public Object getOwnSlotValue(Slot slot) {
        return getDefaultKnowledgeBase().getOwnSlotValue(this, slot);
    }

    public int getOwnSlotValueCount(Slot slot) {
        return getDefaultKnowledgeBase().getOwnSlotValueCount(this, slot);
    }

    public Collection getOwnSlotValues(Slot slot) {
        return getDefaultKnowledgeBase().getOwnSlotValues(this, slot);
    }

    public ValueType getOwnSlotValueType(Slot slot) {
        return getDefaultKnowledgeBase().getOwnSlotValueType(this, slot);
    }

    public Project getProject() {
        return _knowledgeBase.getProject();
    }

    public Collection getReferences() {
        return getReferences(0);
    }

    public Collection getReferences(int maxReferences) {
        return getDefaultKnowledgeBase().getReferences(this, maxReferences);
    }

    public boolean hasOwnSlot(Slot slot) {
        return getDefaultKnowledgeBase().hasOwnSlot(this, slot);
    }

    public boolean isEditable() {
        return _isEditable && !_isIncluded && (getProject() == null || !getProject().isReadonly());
    }

    public boolean isIncluded() {
        return _isIncluded;
    }

    public boolean isSystem() {
        return _isSystem;
    }

    public boolean isValid() {
        return _frameID != null;
    }

    public boolean isValidOwnSlotValue(Slot slot, Object o) {
        return getDefaultKnowledgeBase().isValidOwnSlotValue(this, slot, o);
    }

    public boolean isVisible() {
        return !getProject().isHidden(this);
    }

    public void moveOwnSlotValue(Slot slot, int fromIndex, int toIndex) {
        getDefaultKnowledgeBase().moveOwnSlotValue(this, slot, fromIndex, toIndex);
    }

    public void removeFrameID() {
        if (_frameID == null) {
            Log.warning("duplicate call", this, "removeFrameID");
        } else {
            _frameID = null;
        }
    }

    public void removeFrameListener(FrameListener listener) {
        getDefaultKnowledgeBase().removeFrameListener(this, listener);
    }

    public void removeOwnSlotValue(Slot slot, Object value) {
        getDefaultKnowledgeBase().removeOwnSlotValue(this, slot, value);
    }

    public void setDocumentation(String documentation) {
        getDefaultKnowledgeBase().setDocumentation(this, documentation);
    }

    public void setDocumentation(Collection documentation) {
        getDefaultKnowledgeBase().setDocumentation(this, documentation);
    }

    public void setEditable(boolean b) {
        _isEditable = b;
    }

    public void setIncluded(boolean b) {
        _isIncluded = b;
    }

    public void setName(String newName) {
        getDefaultKnowledgeBase().changeFrameName(this, newName);
    }

    public void setOwnFacetValue(Slot slot, Facet facet, Object value) {
        Assert.fail("not implemented");
    }

    public void setOwnFacetValues(Slot slot, Facet facet, Collection values) {
        Assert.fail("not implemented");
    }

    public void setOwnSlotValue(Slot slot, Object value) {
        setOwnSlotValues(slot, CollectionUtilities.createCollection(value));
    }

    public void setOwnSlotValues(Slot slot, Collection values) {
        getDefaultKnowledgeBase().setOwnSlotValues(this, slot, values);
    }

    public void setSystem(boolean b) {
        _isSystem = b;
    }

    public void setVisible(boolean v) {
        getProject().setHidden(this, !v);
        getDefaultKnowledgeBase().notifyVisibilityChanged(this);
    }

    public String toString() {
        StringBuffer buffer = new StringBuffer();
        buffer.append("Frame(");
        buffer.append(getName());
        buffer.append(")");
        return buffer.toString();
    }
}
