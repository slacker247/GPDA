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
public class ClsEvent extends AbstractEvent {
    private final static int BASE = 300;
    public final static int DIRECT_SUPERCLASS_ADDED = BASE + 1;
    public final static int DIRECT_SUPERCLASS_REMOVED = BASE + 2;
    public final static int DIRECT_SUBCLASS_ADDED = BASE + 3;
    public final static int DIRECT_SUBCLASS_REMOVED = BASE + 4;
    public final static int DIRECT_INSTANCE_CREATED = BASE + 5;
    public final static int DIRECT_INSTANCE_DELETED = BASE + 6;
    public final static int DIRECT_SUBCLASS_MOVED = BASE + 7;

    public final static int TEMPLATE_SLOT_ADDED = BASE + 8;
    public final static int TEMPLATE_SLOT_REMOVED = BASE + 9;
    public final static int TEMPLATE_SLOT_VALUE_CHANGED = BASE + 10;
    public final static int TEMPLATE_FACET_ADDED = BASE + 11;
    public final static int TEMPLATE_FACET_REMOVED = BASE + 12;
    public final static int TEMPLATE_FACET_VALUE_CHANGED = BASE + 13;

    public ClsEvent(Cls cls, int type, Object argument) {
        super(cls, type, argument);
    }

    public ClsEvent(Cls cls, int type, Object argument1, Object argument2) {
        super(cls, type, argument1, argument2);
    }

    public Cls getCls() {
        return (Cls) getSource();
    }

    public Facet getFacet() {
        return (Facet) getArgument2();
    }

    public Instance getInstance() {
        return (Instance) getArgument1();
    }

    public Slot getSlot() {
        return (Slot) getArgument1();
    }

    public Cls getSubclass() {
        Cls cls;
        Object o = getArgument1();
        if (o instanceof Cls) {
            cls = (Cls) o;
        } else {
            cls = null;
            Log.error("invalid cls: " + o, this, "getSubclass");
        }
        return cls;
    }

    public Cls getSuperclass() {
        return (Cls) getArgument1();
    }
}
