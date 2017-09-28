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
public class ClsEventDispatcher implements EventDispatcher {

    public void postEvent(Collection listeners, Object source, int type, Object arg1, Object arg2, Object arg3) {
        ClsEvent event = new ClsEvent((Cls) source, type, arg1, arg2);
        Iterator i = listeners.iterator();
        while (i.hasNext()) {
            ClsListener listener = (ClsListener) i.next();
            switch (type) {
                case ClsEvent.DIRECT_SUPERCLASS_ADDED :
                    listener.directSuperclassAdded(event);
                    break;
                case ClsEvent.DIRECT_SUPERCLASS_REMOVED :
                    listener.directSuperclassRemoved(event);
                    break;
                case ClsEvent.DIRECT_SUBCLASS_ADDED :
                    listener.directSubclassAdded(event);
                    break;
                case ClsEvent.DIRECT_SUBCLASS_REMOVED :
                    listener.directSubclassRemoved(event);
                    break;
                case ClsEvent.DIRECT_SUBCLASS_MOVED :
                    listener.directSubclassMoved(event);
                    break;
                case ClsEvent.DIRECT_INSTANCE_CREATED :
                    listener.directInstanceCreated(event);
                    break;
                case ClsEvent.DIRECT_INSTANCE_DELETED :
                    listener.directInstanceDeleted(event);
                    break;
                case ClsEvent.TEMPLATE_SLOT_ADDED :
                    listener.templateSlotAdded(event);
                    break;
                case ClsEvent.TEMPLATE_SLOT_REMOVED :
                    listener.templateSlotRemoved(event);
                    break;
                case ClsEvent.TEMPLATE_SLOT_VALUE_CHANGED :
                    listener.templateSlotValueChanged(event);
                    break;
                case ClsEvent.TEMPLATE_FACET_ADDED :
                    listener.templateFacetAdded(event);
                    break;
                case ClsEvent.TEMPLATE_FACET_REMOVED :
                    listener.templateFacetRemoved(event);
                    break;
                case ClsEvent.TEMPLATE_FACET_VALUE_CHANGED :
                    listener.templateFacetValueChanged(event);
                    break;
                default :
                    Assert.fail("bad type: " + type);
                    break;
            }
        }
    }
}
