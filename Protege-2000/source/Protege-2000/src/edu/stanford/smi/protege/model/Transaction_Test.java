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
import edu.stanford.smi.protege.util.*;

/**
 * Description of Type
 *
 * @author Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class Transaction_Test extends DefaultTestCase {

    public Transaction_Test(String s) {
        super(s);
    }

    public static void main(String[] args) {
        run(Transaction_Test.class);
    }

    public void setUp() {
        setDatabaseProject();
    }

    public void tearDown() {
        setFileProject();
    }

    public void testCommit() {

        int initialFrameCount = getFrameCount();
        Transaction t = new Transaction(getDomainKB()) {
            public boolean doOperations() {
                KnowledgeBase kb = getKnowledgeBase();
                Cls cls = kb.createCls(null, kb.getRootClses());
                kb.createInstance(null, cls);
                // commit the transaction
                return true;
            }
        };

        boolean succeeded = t.execute();
        assertTrue("transaction succeeded", succeeded);
        assertEquals("frame count in cache", initialFrameCount + 2, getFrameCount());
        saveAndReload();
        assertEquals("frame count in database", initialFrameCount + 2, getFrameCount());
    }

    public void testRollback() {
        String clsName = createCls().getName();

        int initialFrameCount = getFrameCount();

        Transaction t = new Transaction(getDomainKB()) {
            public boolean doOperations() {
                KnowledgeBase kb = getKnowledgeBase();
                Cls cls = kb.createCls(null, kb.getRootClses());
                kb.createInstance(null, cls);
                // rollback this tranaction
                return false;
            }
        };

        boolean committed = t.execute();
        assertTrue("transaction rolled back", !committed);
        assertEquals("frame count in cache", initialFrameCount, getFrameCount());
        saveAndReload();
        assertEquals("frame count in database", initialFrameCount, getFrameCount());
        Cls cls = getCls(clsName);
        assertNotNull("class", cls);
        createInstance(cls);
    }
}
