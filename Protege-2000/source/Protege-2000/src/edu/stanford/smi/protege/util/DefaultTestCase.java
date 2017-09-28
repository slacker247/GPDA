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

package edu.stanford.smi.protege.util;

import java.awt.*;
import java.io.*;
import java.util.*;
import javax.swing.*;
import edu.stanford.smi.protege.event.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.storage.jdbc.*;
import edu.stanford.smi.protege.storage.clips.*;
import edu.stanford.smi.protege.model.Frame;
/**
 * junit TestCase class with some helper methods to project and kb access.
 *
 * @author Ray Fergerson <fergerson@smi.stanford.edu>
 */
public abstract class DefaultTestCase extends junit.framework.TestCase {
    private AbstractEvent _firedEvent;

    private static Project _scratchFileProject;
    private static Project _scratchDatabaseProject;

    private static boolean _isFileProject = true;
    private static boolean _reloadingEnabled = true;

    private static final String SCRATCH_PATH;

    static {
        String testPath = System.getProperty("test.dir", "c:\\projects\\protege-2000\\tests");
        SCRATCH_PATH = new File(testPath, "scratch").getPath();
    }

    protected DefaultTestCase(String name) {
        super(name);
    }

    protected void assertEventFired(int type) {
        assertNotNull("event fired", _firedEvent);
        assertEquals("event fire type", type, _firedEvent.getEventType());
    }

    protected void clearEvents() {
        _firedEvent = null;
    }

    private static void configureForAccess(PropertyList sources) {
        JdbcKnowledgeBaseFactory.setDriver(sources, "sun.jdbc.odbc.JdbcOdbcDriver");
        JdbcKnowledgeBaseFactory.setTablename(sources, "scratch");
        JdbcKnowledgeBaseFactory.setUsername(sources, "rwf");
        JdbcKnowledgeBaseFactory.setURL(sources, "jdbc:odbc:access");
    }

    private static void configureForOracle(PropertyList sources) {
        JdbcKnowledgeBaseFactory.setDriver(sources, "oracle.jdbc.driver.OracleDriver");
        JdbcKnowledgeBaseFactory.setTablename(sources, "scratch");
        JdbcKnowledgeBaseFactory.setUsername(sources, "rwf");
        JdbcKnowledgeBaseFactory.setPassword(sources, "ray");
        JdbcKnowledgeBaseFactory.setURL(sources, "jdbc:oracle:thin:@pharmgate.stanford.edu:1521:PHARM");
    }

    protected Cls createCls() {
        return createCls(null);
    }

    protected Cls createCls(String name) {
        return createCls(name, getDomainKB().getRootCls());
    }

    protected Cls createCls(String name, Cls parent) {
        Cls cls = getDomainKB().createCls(name, CollectionUtilities.createCollection(parent));
        // Log.trace("class= " + cls, this, "createCls", name, parent);
        return cls;
    }

    protected Facet createFacet() {
        return getDomainKB().createFacet(null);
    }

    protected Frame createFrame() {
        return createCls();
    }

    protected Instance createInstance(Cls cls) {
        Instance instance = getDomainKB().createInstance(null, cls);
        if (instance instanceof Cls) {
            ((Cls) instance).addDirectSuperclass(getDomainKB().getRootCls());
            // Log.trace("instance=" + instance, this, "createInstance", cls);
        }
        return instance;
    }

    protected Slot createMultiValuedSlot(ValueType type) {
        Slot slot = getDomainKB().createSlot(null);
        slot.setValueType(type);
        slot.setAllowsMultipleValues(true);
        return slot;
    }

    protected Slot createMultiValuedSlot(ValueType type, Cls cls) {
        Slot slot = createMultiValuedSlot(type);
        setCompleteValueType(cls, slot, type);
        return slot;
    }

    protected Slot createSingleValuedSlot(ValueType type) {
        Slot slot = getDomainKB().createSlot(null);
        slot.setValueType(type);
        return slot;
    }

    protected Slot createSingleValuedSlot(ValueType type, Cls cls) {
        Slot slot = createSingleValuedSlot(type);
        setCompleteValueType(cls, slot, type);
        return slot;
    }

    protected Cls createSubCls(Cls parent) {
        return createCls(null, parent);
    }

    protected Slot createSubSlot(Slot parent) {
        return getDomainKB().createSlot(null, parent.getDirectType(), Collections.singleton(parent), true);
    }

    protected void deleteFrame(Frame frame) {
        getDomainKB().deleteFrame(frame);
    }

    private static void fakeSaveAndReloadEvents(Project p) {
        p.postProjectEvent(ProjectEvent.PROJECT_SAVED);
        p.postProjectEvent(ProjectEvent.PROJECT_CLOSED);
    }

    protected Cls getCls(String name) {
        return getDomainKB().getCls(name);
    }

    protected KnowledgeBase getDomainKB() {
        return getProject().getKnowledgeBase();
    }

    protected Facet getFacet(String name) {
        return getDomainKB().getFacet(name);
    }

    public Frame getFrame(String name) {
        return getDomainKB().getFrame(name);
    }

    protected int getFrameCount() {
        return getDomainKB().getFrameCount();
    }

    protected Instance getInstance(String name) {
        return getDomainKB().getInstance(name);
    }

    protected Project getProject() {
        return (_isFileProject) ? getScratchFileProject() : getScratchDatabaseProject();
    }

    protected Map getPropertyMap() {
        return getProject().getPropertyMap();
    }

    private Project getScratchDatabaseProject() {
        if (_scratchDatabaseProject == null) {
            ArrayList errors = new ArrayList();
            Project project = new Project(null, errors);
            String path = getScratchPath("scratch_project", "scratch_db.pprj").toString();
            project.setProjectFilePath(path);
            PropertyList sources = project.getSources();
            sources.setString(KnowledgeBaseFactory.FACTORY_CLASS_NAME, JdbcKnowledgeBaseFactory.class.getName());
            // configureForAccess(sources);
            configureForOracle(sources);
            project.save(errors);
            _scratchDatabaseProject = new Project(path, errors);
            handleErrors(errors);
        }
        return _scratchDatabaseProject;
    }

    private Project getScratchFileProject() {
        if (_scratchFileProject == null) {
            Collection errors = new ArrayList();
            _scratchFileProject = new Project(null, errors);
            handleErrors(errors);
            String path = getScratchPath("scratch_project", "scratch.pprj").toString();
            _scratchFileProject.setProjectFilePath(path);
        }
        return _scratchFileProject;
    }

    public static File getScratchPath(String dir, String file) {
        File f = new File(SCRATCH_PATH, dir);
        f.mkdirs();
        f = new File(f, file);
        return f;
    }

    protected Slot getSlot(String name) {
        return getDomainKB().getSlot(name);
    }

    private static void handleErrors(Collection errors) {
        if (errors.size() != 0) {
            int count = 0;
            Iterator i = errors.iterator();
            while (i.hasNext()) {
                Object o = i.next();
                String text;
                if (o instanceof Exception) {
                    Log.exception((Exception) o, DefaultTestCase.class, "saveAndReloadProject");
                } else {
                    Log.error(count++ +": " + o, DefaultTestCase.class, "saveAndReloadProject");
                }
            }
            assertEquals("errors", 0, errors.size());
        }
    }

    public static void init() {
        _scratchFileProject = null;
        _scratchDatabaseProject = null;
    }

    public void pressButton(Component c, Icon icon) {
        ComponentUtilities.pressButton(c, icon);
    }

    public void recordEventFired(AbstractEvent event) {
        _firedEvent = event;
    }

    public static void run(Class c) {
        junit.swingui.TestRunner.run(c);
    }

    protected void saveAndReload() {
        if (_reloadingEnabled) {
            int initialFrameCount = getFrameCount();
            if (_isFileProject) {
                if (_scratchFileProject != null) {
                    _scratchFileProject = saveAndReloadProject(_scratchFileProject);
                }
            } else {
                if (_scratchDatabaseProject != null) {
                    _scratchDatabaseProject = saveAndReloadProject(_scratchDatabaseProject);
                }
            }
            int finalFrameCount = getFrameCount();
            assertEquals("frame count", initialFrameCount, finalFrameCount);
        } else {
            // at least we have to fake the events
            if (_isFileProject) {
                fakeSaveAndReloadEvents(_scratchFileProject);
            } else {
                fakeSaveAndReloadEvents(_scratchDatabaseProject);
            }
        }
    }

    private static Project saveAndReloadProject(Project project) {
        String path = project.getProjectFilePath();

        // hack to take care of static path problem in FileUtilities
        FileUtilities.setCurrentWorkingDirectoryFromFile(path);

        Collection errors = new ArrayList();
        project.save(errors);
        project.dispose();
        Project p = new Project(path, errors);
        handleErrors(errors);
        return p;
    }

    private void setCompleteValueType(Cls cls, Slot slot, ValueType type) {
        if (type == ValueType.INSTANCE) {
            slot.setAllowedClses(Collections.singleton(cls));
        } else if (type == ValueType.CLS) {
            slot.setAllowedParents(Collections.singleton(cls));
        } else {
            fail("bad type: " + type);
        }
    }

    protected void setDatabaseProject() {
        _isFileProject = false;
    }

    protected void setFileProject() {
        _isFileProject = true;
    }

    public static void setReloadingEnabled(boolean b) {
        _reloadingEnabled = b;
    }

    public void setUp() {
    }

    public void tearDown() {
        // Log.enter(this, "tearDown");

        // This improves test case isolation but hurts performance
        // because it forces new project creation on every test
        // init();

        setFileProject();
        clearEvents();
    }
}
