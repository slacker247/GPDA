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

/**
 *  interface for notification of knowledge-base events The frame that has been
 *  added or removed from the knowledge-base is available through the
 *  KnowledgeBaseEvent.getFrame() call.
 *
 * @author Ray Fergerson <fergerson@smi.stanford.edu>
 */
public interface KnowledgeBaseListener extends EventListener {

    void clsCreated(KnowledgeBaseEvent event);

    void clsDeleted(KnowledgeBaseEvent event);

    void defaultClsMetaClsChanged(KnowledgeBaseEvent event);

    void defaultFacetMetaClsChanged(KnowledgeBaseEvent event);

    void defaultSlotMetaClsChanged(KnowledgeBaseEvent event);

    void facetCreated(KnowledgeBaseEvent event);

    void facetDeleted(KnowledgeBaseEvent event);

    void frameNameChanged(KnowledgeBaseEvent event);

    void instanceCreated(KnowledgeBaseEvent event);

    void instanceDeleted(KnowledgeBaseEvent event);

    void slotCreated(KnowledgeBaseEvent event);

    void slotDeleted(KnowledgeBaseEvent event);
}
