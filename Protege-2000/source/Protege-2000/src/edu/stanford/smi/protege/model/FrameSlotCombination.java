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

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class FrameSlotCombination {
    private Frame _frame;
    private Slot _slot;

    public FrameSlotCombination(Frame frame, Slot slot) {
        this._frame = frame;
        this._slot = slot;
    }

    public boolean equals(Object o) {
        boolean equals;
        if (o instanceof FrameSlotCombination) {
            FrameSlotCombination other = (FrameSlotCombination) o;
            equals = (other._frame == _frame && other._slot == _slot);
        } else {
            equals = false;
        }
        return equals;
    }

    public Frame getFrame() {
        return _frame;
    }

    public Slot getSlot() {
        return _slot;
    }

    public int hashCode() {
        return _frame.hashCode() ^ _slot.hashCode();
    }
}
