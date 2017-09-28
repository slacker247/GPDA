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

import java.util.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.util.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class SlotEventDispatcher implements EventDispatcher {

    public void postEvent(Collection listeners, Object source, int type, Object arg1, Object arg2, Object arg3) {
        SlotEvent event = new SlotEvent((Slot) source, type, (Frame) arg1);
        Iterator i = listeners.iterator();
        while (i.hasNext()) {
            SlotListener listener = (SlotListener) i.next();
            switch (type) {
                case SlotEvent.OWN_SLOT_FRAME_ADDED :
                    listener.ownSlotFrameAdded(event);
                    break;
                case SlotEvent.OWN_SLOT_FRAME_REMOVED :
                    listener.ownSlotFrameRemoved(event);
                    break;
                case SlotEvent.TEMPLATE_SLOT_CLS_ADDED :
                    listener.templateSlotClsAdded(event);
                    break;
                case SlotEvent.TEMPLATE_SLOT_CLS_REMOVED :
                    listener.templateSlotClsRemoved(event);
                    break;
                default :
                    Assert.fail("bad type: " + type);
                    break;
            }
        }
    }
}
