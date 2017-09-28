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

package edu.stanford.smi.protege.ui;

import java.awt.*;
import java.util.*;
import javax.swing.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.resource.*;
import edu.stanford.smi.protege.util.*;
/**
 * Description of Type
 *
 * @author Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class InstanceDisplay_Test extends DefaultTestCase {

    public InstanceDisplay_Test(String name) {
        super(name);
    }

    private void deleteAllStickies() {
        KnowledgeBase kb = getDomainKB();
        Cls cls = kb.getCls(Model.Cls.INSTANCE_ANNOTATION);
        Iterator i = new ArrayList(cls.getInstances()).iterator();
        while (i.hasNext()) {
            Instance instance = (Instance) i.next();
            kb.deleteInstance(instance);
        }
    }

    private JFrame loadIntoFrame(String instanceName) {
        JFrame frame1 = new JFrame();
        InstanceDisplay id = new InstanceDisplay(getProject());
        frame1.getContentPane().add(id);
        Instance instance = getInstance(instanceName);
        assertNotNull("instance exists: " + instanceName, instance);
        id.setInstance(instance);
        frame1.pack();
        frame1.show();
        Toolkit.getDefaultToolkit().sync();
        return frame1;
    }

    public static void main(String[] args) {
        run(InstanceDisplay_Test.class);
    }

    private void pressMessageOK() {
        Thread t = new Thread() {
            public void run() {
                JDialog dialog = null;
                while (dialog == null) {
                    dialog = ModalDialog.getCurrentDialog();
                    if (dialog != null) {
                        pressButton(dialog, Icons.getYesIcon());
                        return;
                    }
                    SystemUtilities.sleepMsec(100);
                }
            }
        };
        t.setDaemon(true);
        t.start();
    }

    public void testYellowStickyCreation() {
        deleteAllStickies();
        String frameName = createCls().getName();
        JFrame frame1 = loadIntoFrame(frameName);
        int frameCount = getFrameCount();
        pressButton(frame1, Icons.getCreateYellowStickyIcon());
        assertEquals("frame count", frameCount + 1, getFrameCount());
        JInternalFrame inf = (JInternalFrame) ComponentUtilities.getDescendentOfClass(JInternalFrame.class, frame1);
        assertNotNull("internal frame", inf);
        JTextArea area = (JTextArea) ComponentUtilities.getDescendentOfClass(JTextArea.class, inf);
        assertNotNull("text area", area);
        area.setText("This is a test - " + System.currentTimeMillis());

        JFrame frame2 = loadIntoFrame(frameName);
        Point loc = frame2.getLocation();
        loc.x += 200;
        frame2.setLocation(loc);

        ComponentUtilities.dispose(frame1);
        ComponentUtilities.dispose(frame2);
        deleteAllStickies();
    }

    public void testYellowStickyDeletion() {
        deleteAllStickies();
        String frameName = createCls().getName();
        JFrame frame = loadIntoFrame(frameName);
        int count = getFrameCount();
        pressButton(frame, Icons.getCreateYellowStickyIcon());
        assertEquals("added sticky", count + 1, getFrameCount());
        pressMessageOK();
        pressButton(frame, Icons.getDeleteYellowStickyIcon());
        assertEquals("deleted sticky", count, getFrameCount());
        ComponentUtilities.dispose(frame);
        deleteAllStickies();
    }

    public void testYellowStickyLocationSave() {
        String frameName = createCls().getName();
        JFrame frame1 = loadIntoFrame(frameName);
        pressButton(frame1, Icons.getCreateYellowStickyIcon());
        JInternalFrame iFrame = (JInternalFrame) ComponentUtilities.getDescendentOfClass(JInternalFrame.class, frame1);
        JTextArea area = (JTextArea) ComponentUtilities.getDescendentOfClass(JTextArea.class, iFrame);
        area.setText("This is a test - " + System.currentTimeMillis());
        Point loc = iFrame.getLocation();
        loc.x += 100;
        iFrame.setLocation(loc);
        ComponentUtilities.dispose(frame1);

        JFrame frame2 = loadIntoFrame(frameName);
        JInternalFrame iFrame2 = (JInternalFrame) ComponentUtilities.getDescendentOfClass(JInternalFrame.class, frame2);
        assertEquals("within project", loc, iFrame2.getLocation());
        ComponentUtilities.dispose(frame2);

        saveAndReload();
        JFrame frame3 = loadIntoFrame(frameName);
        JInternalFrame iFrame3 = (JInternalFrame) ComponentUtilities.getDescendentOfClass(JInternalFrame.class, frame3);
        assertEquals("after reload", loc, iFrame3.getLocation());
        ComponentUtilities.dispose(frame3);

        deleteAllStickies();
    }
}
