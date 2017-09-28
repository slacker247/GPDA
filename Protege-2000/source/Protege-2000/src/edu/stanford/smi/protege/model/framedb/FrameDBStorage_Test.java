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

package edu.stanford.smi.protege.model.framedb;

import java.util.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.util.*;

/**
 * Description of Type
 *
 * @author Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class FrameDBStorage_Test extends DefaultTestCase {

    public FrameDBStorage_Test(String name) {
        super(name);
    }

    public static void main(String[] args) {
        run(FrameDBStorage_Test.class);
    }

    public void testCaching() {
        final int N_FRAMES = 10;
        FrameID[] frameIDs = new FrameID[N_FRAMES];

        DefaultKnowledgeBase kb = new DefaultKnowledgeBase(null);
        Collection rootClses = kb.getRootClses();
        int startCount = kb.getFrameCount();
        FrameDBStorage storage = (FrameDBStorage) kb.getStorage();
        storage.setCaching(true);
        storage.setUseWeakReference(true);
        for (int i = 0; i < N_FRAMES; ++i) {
            Cls cls = kb.createCls(null, rootClses);
            frameIDs[i] = cls.getFrameID();
            Thread.yield();
        }
        SystemUtilities.gc();
        int cleared = 0;
        for (int i = 0; i < N_FRAMES; ++i) {
            Frame frame = kb.getFrame(frameIDs[i]);
            if (frame == null) {
                ++cleared;
            }
        }
        // Log.trace("cleared " + cleared, this, "testCaching");
        assertTrue("flush failed", cleared > 0);
    }
}
