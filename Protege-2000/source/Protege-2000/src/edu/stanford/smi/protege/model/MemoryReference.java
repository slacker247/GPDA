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


import edu.stanford.smi.protege.util.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class MemoryReference implements Reference {
    protected Frame _frame;
    protected Slot _slot;
    protected Facet _facet;
    protected boolean _isTemplate;
    private int _hashCode;

    public MemoryReference() {
    }

    public MemoryReference(Frame frame, Slot slot, Facet facet, boolean template) {
        init(frame, slot, facet, template);
    }

    public boolean equals(Object o) {
        boolean equals;
        if (o instanceof MemoryReference) {
            MemoryReference r = (MemoryReference) o;
            if (_hashCode == r._hashCode) {
                equals =
                        _frame == r._frame &&
                        _slot == r._slot &&
                        _facet == r._facet &&
                        _isTemplate == r._isTemplate;
            } else {
                equals = false;
            }
        } else {
            equals = false;
        }
        return equals;
    }

    public Facet getFacet() {
        return _facet;
    }

    public Frame getFrame() {
        return _frame;
    }

    public Slot getSlot() {
        return _slot;
    }

    public int hashCode() {
        return _hashCode;
    }

    protected void init(Frame frame, Slot slot, Facet facet, boolean template) {
        Assert.assertNotNull("frame", frame);
        Assert.assertNotNull("slot", slot);
        this._frame = frame;
        this._slot = slot;
        this._facet = facet;
        this._isTemplate = template;
        _hashCode = frame.hashCode() ^ slot.hashCode() + (_isTemplate ? 0 : 1);
        if (facet != null) {
            _hashCode ^= facet.hashCode();
        }
    }

    public boolean isTemplate() {
        return _isTemplate;
    }

    public String toString() {
        StringBuffer buf = new StringBuffer();
        buf.append("MemoryReference(");
        buf.append(_frame.getName());
        buf.append(", ");
        buf.append(_slot.getName());
        if (_facet != null) {
            buf.append(", ");
            buf.append(_facet.getName());
        }
        buf.append(")");
        return buf.toString();
    }
}
