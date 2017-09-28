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

package edu.stanford.smi.protege.storage.jdbc;



import edu.stanford.smi.protege.model.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class DBReference {
    private FrameID _frame;
    private FrameID _slot;
    private FrameID _facet;
    private boolean _isTemplate;

    public DBReference(Frame frame, Slot slot, Facet facet, boolean isTemplate) {
        _frame = frame.getFrameID();
        _slot = slot.getFrameID();
        _facet = (facet == null) ? (FrameID) null : facet.getFrameID();
        _isTemplate = isTemplate;
    }

    public DBReference(FrameID frame, FrameID slot, FrameID facet, boolean isTemplate) {
        _frame = frame;
        _slot = slot;
        _facet = facet;
        _isTemplate = isTemplate;
    }

    public boolean equals(Object o) {
        DBReference other = (DBReference) o;
        return getFrame() == other._frame && getSlot() == other._slot && getFacet() == other._facet && getIsTemplate() == other._isTemplate;
    }

    /**
     * Return the value of the field facet
     */
    public FrameID getFacet() {
        return _facet;
    }

    /**
     * Return the value of the field frame
     */
    public FrameID getFrame() {
        return _frame;
    }

    /**
     * Return the value of the field isTemplate
     */
    public boolean getIsTemplate() {
        return _isTemplate;
    }

    /**
     * Return the value of the field slot
     */
    public FrameID getSlot() {
        return _slot;
    }

    public int hashCode() {
        int result = getFrame().hashCode() ^ getSlot().hashCode();
        if (getFacet() != null) {
            result ^= getFacet().hashCode();
        }
        result += getIsTemplate() ? 0 : 1;
        return result;
    }
}
