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

import java.awt.*;
import java.util.*;
import edu.stanford.smi.protege.event.*;
import edu.stanford.smi.protege.util.*;
/**
 * JUnit test case for Project
 *
 * @author Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class Project_Test extends DefaultTestCase {

    public Project_Test(String name) {
        super(name);
    }

    public static void main(String[] args) {
        run(Project_Test.class);
    }

    public void testPropertyMapLoading() {
        Frame frame = createFrame();
        String frameName = frame.getName();
        Map map = getPropertyMap();
        Map testMap = new HashMap();
        map.put("testmap", testMap);
        Rectangle savedRect = new Rectangle(1, 2, 3, 4);
        testMap.put(frame, savedRect);
        testMap.put("foo", "bar");

        saveAndReload();

        frame = getFrame(frameName);
        testMap = (Map) getPropertyMap().get("testmap");
        assertNotNull("map exists", testMap);
        assertEquals("string", "bar", testMap.get("foo"));
        assertEquals("rectangle", savedRect, testMap.get(frame));
    }
}
