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

package edu.stanford.smi.protege.event;

import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.util.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class FrameEvent extends AbstractEvent {
    private final static int BASE = 100;
    public final static int NAME_CHANGED = BASE + 1;
    public final static int DELETED = BASE + 2;
    public final static int VISIBILITY_CHANGED = BASE + 3;
    public final static int BROWSER_TEXT_CHANGED = BASE + 5;

    public final static int OWN_SLOT_ADDED = BASE + 6;
    public final static int OWN_SLOT_REMOVED = BASE + 7;
    public final static int OWN_FACET_ADDED = BASE + 8;
    public final static int OWN_FACET_REMOVED = BASE + 9;
    public final static int OWN_SLOT_VALUE_CHANGED = BASE + 10;
    public final static int OWN_FACET_VALUE_CHANGED = BASE + 11;

    public FrameEvent(Frame frame, int type) {
        super(frame, type);
    }

    public FrameEvent(Frame frame, int type, Object argument) {
        super(frame, type, argument);
    }

    public FrameEvent(Frame frame, int type, Object argument1, Object argument2) {
        super(frame, type, argument1, argument2);
    }

    public Facet getFacet() {
        return (Facet) getArgument2();
    }

    public Frame getFrame() {
        return (Frame) getSource();
    }

    public String getOldName() {
        return (String) getArgument1();
    }

    public Slot getSlot() {
        return (Slot) getArgument1();
    }
}
