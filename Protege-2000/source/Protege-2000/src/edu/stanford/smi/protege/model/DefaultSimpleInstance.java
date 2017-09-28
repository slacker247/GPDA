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
 * This is the concrete subclass of DefaultInstance which handles everything but classes, slots, and facets.
 * This is also the class to be subclassed by users of the "java_packages" feature.
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class DefaultSimpleInstance extends DefaultInstance implements SimpleInstance {

    /**
     * This constructor should be used by classes which use the "java_packages" feature of
     * protege to load instances of user defined classes.  The User defined classes must
     * have a constructor whose signature (arguments) exactly match this constructor's signature.
     */
    public DefaultSimpleInstance(KnowledgeBase kb, FrameID id) {
        super(kb, id);
    }

    /**
     * This constructor may be called by applications that need to construct instances by calling a
     * constructor directly rather than by calling KnowledgeBase.createInstance (for example, JESS).
     * All other applications should use the KnowledgeBase.createInstance call to make instances.
     */
    public DefaultSimpleInstance(KnowledgeBase kb, String name, Cls cls) {
        super(kb, null);
        getDefaultKnowledgeBase().addInstance(this, name, cls, true);
    }
}
