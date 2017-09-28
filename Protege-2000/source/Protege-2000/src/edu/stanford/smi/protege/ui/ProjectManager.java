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
import java.io.*;
import java.util.*;
import javax.swing.*;
import edu.stanford.smi.protege.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.resource.*;
import edu.stanford.smi.protege.storage.clips.*;
import edu.stanford.smi.protege.ui.*;
import edu.stanford.smi.protege.util.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class ProjectManager {
    private static ProjectManager _projectManager = new ProjectManager();
    private Project _currentProject;
    private JRootPane _rootPane;
    private boolean _doExitVM = true;

    private static class FactoryPanel extends JPanel {
        private JList itsList;

        public FactoryPanel() {
            itsList = ComponentFactory.createList(null);
            itsList.setCellRenderer(new FactoryRenderer());
            ComponentUtilities.setListValues(itsList, SystemUtilities.getAvailableFactories());
            itsList.setSelectedIndex(0);
            setLayout(new BorderLayout());
            add(ComponentFactory.createScrollPane(itsList));
            setPreferredSize(new Dimension(300, 100));
        }

        public KnowledgeBaseFactory getSelectedFactory() {
            return (KnowledgeBaseFactory) itsList.getSelectedValue();
        }
    }

    private static class FactoryRenderer extends DefaultRenderer {
        public void load(Object o) {
            KnowledgeBaseFactory f = (KnowledgeBaseFactory) o;
            setMainText(f.getDescription());
        }
    }

    public ProjectManager() {
    }

    private static void advance(Point p) {
        p.x += 25;
        p.y += 25;
    }

    public void buildProjectRequest() {
        if (closeProjectRequest()) {
            KnowledgeBaseFactory factory = promptForFactory();
            Collection errors = new ArrayList();
            Project p = Project.createBuildProject(factory, errors);
            if (loadNewSources(p, factory, false)) {
                WaitCursor waitCursor = new WaitCursor(_rootPane);
                try {
                    p.createDomainKnowledgeBase(factory, errors, true);
                    _currentProject = p;
                    displayCurrentProject();
                } finally {
                    waitCursor.hide();
                }
                displayErrors("Build Project Errors", errors);
            }
        }
    }

    public void cascadeWindows(Point p, Collection w) {
        ArrayList windows = new ArrayList(w);
        Collections.sort(windows, new WindowComparator());
        Iterator i = windows.iterator();
        while (i.hasNext()) {
            Window window = (Window) i.next();
            window.setLocation(p);
            window.toFront();
            window.requestFocus();
            advance(p);
        }
    }

    public void cascadeWindowsRequest() {
        Project project = getCurrentProject();
        if (project != null) {
            Point startPoint = SwingUtilities.windowForComponent(_rootPane).getLocation();
            startPoint.x += 25;
            startPoint.y += 110;
            cascadeWindows(startPoint, project.getOpenWindows());
        }
    }

    public void changeProjectStorageFormatRequest() {
        if (hasLoadedProject()) {
            KnowledgeBaseFactory factory = promptForFactory();
            boolean succeeded = (factory != null) && factory != _currentProject.getKnowledgeBase().getKnowledgeBaseFactory();
            if (succeeded) {
                succeeded = loadNewSources(_currentProject, factory, true);
            }
            if (succeeded) {
                if (_currentProject.hasCompleteSources()) {
                    succeeded = save();
                    if (succeeded) {
                        String path = _currentProject.getProjectFilePath();
                        unloadProject();
                        loadProject(path);
                    }
                } else {
                    Log.trace("Sources are not complete", this, "saveProjectAsRequest");
                }
            }
        }
    }

    public boolean closeProjectRequest() {
        boolean succeeded = true;
        if (hasLoadedProject()) {
            if (_currentProject.isDirty()) {
                succeeded = confirmSave();
            }
            if (succeeded) {
                succeeded = getCurrentProjectView().attemptClose();
                if (succeeded) {
                    unloadProject();
                }
            }
        }
        return succeeded;
    }

    public void configureProjectRequest() {
        Project p = getCurrentProject();
        if (p != null) {
            ConfigureProjectPanel panel = new ConfigureProjectPanel(p);
            String title = "Configure " + p.getProjectFilePath();
            int result = ModalDialog.showDialog(_rootPane, panel, title, ModalDialog.MODE_OK_CANCEL);
            if (result == ModalDialog.OPTION_OK) {
                reloadUI();
            }
        }
    }

    private boolean confirmSave() {
        boolean succeeded;
        JComponent c = new JLabel("Do you want to save changes to the current project?");
        int result = ModalDialog.showDialog(_rootPane, c, "Save?", ModalDialog.MODE_YES_NO_CANCEL);
        switch (result) {
            case ModalDialog.OPTION_YES :
                succeeded = saveProjectRequest();
                break;
            case ModalDialog.OPTION_NO :
                succeeded = true;
                break;
            case ModalDialog.OPTION_CANCEL :
            case ModalDialog.OPTION_CLOSE :
                succeeded = false;
                break;
            default :
                Assert.fail("bad result: " + result);
                succeeded = false;
                break;
        }
        return succeeded;
    }

    private void displayCurrentProject() {
        // Log.enter(this, "displayCurrentProject");
        WaitCursor waitCursor = new WaitCursor(_rootPane);
        try {
            ProjectView view = new ProjectView(_currentProject);
            _rootPane.getContentPane().add(view, BorderLayout.CENTER);
            view.revalidate();
            view.repaint();
        } finally {
            waitCursor.hide();
        }
        updateFrameTitle();
        // ComponentUtilities.pack(itsRootPane);
        // Log.exit(this, "displayCurrentProject");
    }

    private void displayErrors(String label, Collection errors) {
        if (!errors.isEmpty()) {
            JComponent panel = new ParseErrorPanel(errors);
            ComponentFactory.showInFrame(panel, label);
        }
    }

    private void displayIOException(IOException e) {
        String text = "IO error: " + e.getMessage();
        ModalDialog.showMessageDialog(_rootPane, text);
    }

    private void disposeProjectView() {
        ProjectView view = getCurrentProjectView();
        if (view != null) {
            ComponentUtilities.dispose(view);
            Journal.enter(this, "close project");
            Journal.stopRecording();
        }
    }

    public void exitApplicationRequest() {
        boolean succeeded = closeProjectRequest();
        if (succeeded) {
            java.awt.Frame mainFrame = ComponentUtilities.getFrame(_rootPane);
            ApplicationProperties.recordMainFrameProperties(mainFrame);
            ApplicationProperties.flush();
            ComponentUtilities.dispose(mainFrame);
            if (_doExitVM) {
                SystemUtilities.exit();
            }
        }
    }

    public Project getCurrentProject() {
        return _currentProject;
    }

    public ProjectView getCurrentProjectView() {
        ProjectView view;
        Container c = _rootPane.getContentPane();
        if (c.getComponentCount() >= 2) {
            view = (ProjectView) c.getComponent(1);
        } else {
            view = null;
        }
        return view;
    }

    private Collection getKnowledgeBaseFactories() {
        Collection factories = new ArrayList();
        factories.add(new ClipsKnowledgeBaseFactory());
        return factories;
    }

    public JComponent getMainPanel() {
        return _rootPane;
    }

    public static ProjectManager getProjectManager() {
        return _projectManager;
    }

    public String getRequestedIncludeProject() {
        return getRequestedProject();
    }

    private String getRequestedProject() {
        String projectFilePath;
        JFileChooser chooser = ComponentFactory.createFileChooser("Project", ".pprj");
        int rval = chooser.showOpenDialog(_rootPane);
        if (rval == JFileChooser.APPROVE_OPTION) {
            projectFilePath = chooser.getSelectedFile().getPath();
        } else {
            projectFilePath = null;
        }
        return projectFilePath;
    }

    private boolean hasLoadedProject() {
        return _currentProject != null;
    }

    public void includeProjectRequest() {
        if (hasLoadedProject()) {
            String filePath = getRequestedIncludeProject();
            if (filePath != null) {
                Collection errors = new ArrayList();
                WaitCursor waitCursor = new WaitCursor(_rootPane);
                try {
                    _currentProject.includeProject(filePath, errors);
                } finally {
                    waitCursor.hide();
                }
                displayErrors("Loading Included Project Errors", errors);
            }
        }
    }

    // return true if sources are complete and 'ok' was pressed
    private boolean loadNewSources(Project project, KnowledgeBaseFactory factory, boolean showProject) {
        if (factory == null) {
            factory = project.getKnowledgeBaseFactory();
        }
        PropertyList sources = project.getSources();
        KnowledgeBaseSourcesEditor editor = factory.createKnowledgeBaseSourcesEditor(project.getProjectFilePath(), sources);
        editor.setShowProject(showProject);
        String title = factory.getDescription();
        int result = ModalDialog.showDialog(_rootPane, editor, title, ModalDialog.MODE_OK_CANCEL);
        if (result == ModalDialog.OPTION_OK) {
            sources.setString(KnowledgeBaseFactory.FACTORY_CLASS_NAME, factory.getClass().getName());
            project.setProjectFilePath(editor.getProjectPath());
            Iterator i = editor.getIncludedProjects().iterator();
            while (i.hasNext()) {
                String file = (String) i.next();
                project.includeProject(file, false, null);
            }
        }
        return result == ModalDialog.OPTION_OK;
    }

    public void loadProject(String filePath) {
        long t1 = System.currentTimeMillis();
        Collection errors = new ArrayList();
        WaitCursor waitCursor = new WaitCursor(_rootPane);
        try {
            if (filePath == null) {
                KnowledgeBaseFactory factory = promptForFactory();
                t1 = System.currentTimeMillis(); // reinitialize after prompt to user!
                if (factory != null) {
                    _currentProject = Project.createNewProject(factory, errors);
                }
            } else {
                _currentProject = Project.loadProjectFromFile(filePath, errors);
            }
        } finally {
            waitCursor.hide();
        }
        long t2 = System.currentTimeMillis();
        System.out.println("Load time for " + filePath + " = " + (t2 - t1) / 1000 + " sec");

        if (_currentProject != null) {
            displayErrors("Load Project Errors", errors);
            displayCurrentProject();
        }
    }

    public void loadProject(String filePath, KnowledgeBaseFactory factory) {
        Collection errors = new ArrayList();
        long t1, t2;

        if (factory != null) {
            t1 = System.currentTimeMillis();
            if (filePath == null) {
                _currentProject = Project.createNewProject(factory, errors);
            } else {
                _currentProject = Project.loadProjectFromFile(filePath, errors);
            }
            t2 = System.currentTimeMillis();
            System.out.println("Load time for " + filePath + " = " + (t2 - t1) / 1000 + " sec");
        }

        if (_currentProject != null) {
            displayErrors("Load Project Errors", errors);
            displayCurrentProject();
        }
    }

    public void mergeIncludedProjectsRequest() {
        if (hasLoadedProject() && _currentProject.hasIncludedProjects()) {
            String text =
                "This action will make all included frames in the knowledge base direct members " + " of the current project.";
            JComponent parent = getProjectManager().getMainPanel();
            int rval = ModalDialog.showMessageDialog(parent, text, ModalDialog.MODE_OK_CANCEL);
            if (rval == ModalDialog.OPTION_OK) {
                _currentProject.mergeIncludedProjects();
            }
        }
    }

    public void newProjectRequest() {
        if (closeProjectRequest()) {
            loadProject(null);
        }
    }

    public void openProjectRequest() {
        if (closeProjectRequest()) {
            String filePath = getRequestedProject();
            if (filePath != null) {
                loadProject(filePath);
                ApplicationProperties.addProjectToMRUList(filePath);
            }
        }
    }

    private KnowledgeBaseFactory promptForFactory() {
        FactoryPanel panel = new FactoryPanel();
        int rval = ModalDialog.showDialog(_rootPane, panel, "Select Format", ModalDialog.MODE_OK_CANCEL);
        return (rval == ModalDialog.OPTION_OK) ? panel.getSelectedFactory() : null;
    }

    public void reloadUI() {
        getCurrentProjectView().reload();
        Application.repaint();
    }

    private boolean save() {
        Collection errors = new ArrayList();
        Journal.enter(this, "save project");
        boolean save = getCurrentProjectView().attemptSave();
        if (save) {
            WaitCursor waitCursor = new WaitCursor(_rootPane);
            try {
                _currentProject.save(errors);
            } catch (Exception e) {
                errors.add(e);
            } finally {
                waitCursor.hide();
            }
            displayErrors("Save Project Errors", errors);
        }
        return save && errors.isEmpty();
    }

    public boolean saveProjectAsRequest() {
        return saveProjectAsRequest(null);
    }

    public boolean saveProjectAsRequest(KnowledgeBaseFactory factory) {
        boolean succeeded = true;
        if (hasLoadedProject()) {
            succeeded = loadNewSources(_currentProject, factory, true);
            if (succeeded) {
                if (_currentProject.hasCompleteSources()) {
                    succeeded = save();
                    if (succeeded) {
                        updateFrameTitle();
                    }
                } else {
                    Log.trace("Sources are not complete", this, "saveProjectAsRequest");
                }
            }
        }
        return succeeded;
    }

    public boolean saveProjectRequest() {
        boolean succeeded = true;
        if (hasLoadedProject()) {
            if (_currentProject.hasCompleteSources()) {
                succeeded = save();
            } else {
                succeeded = saveProjectAsRequest(_currentProject.getKnowledgeBaseFactory());
            }
        }
        return succeeded;
    }

    public void setLookAndFeel(LookAndFeel lookAndFeel) {
        try {
            UIManager.setLookAndFeel(lookAndFeel);
            updateUI();
        } catch (Exception e) {
            Log.exception(e, this, "setLookAndFeel", lookAndFeel);
        }
    }

    public void setRootPane(JRootPane rootPane) {
        this._rootPane = rootPane;
        setupRootPane();
    }

    /**
     * Insert the method's description here.
     * Creation date: (8/16/2000 4:17:18 PM)
     */
    void setupRootPane() {
        _rootPane.getContentPane().setLayout(new BorderLayout());
        _rootPane.getContentPane().add(new ProjectToolBar(), BorderLayout.NORTH);
        _rootPane.setJMenuBar(new ProjectMenuBar());
        ComponentUtilities.setFrameTitle(_rootPane, Text.getProgramName());
    }

    public String toString() {
        return "ProjectManager";
    }

    private void unloadProject() {
        ComponentUtilities.closeAllWindows();
        disposeProjectView();
        _rootPane.getContentPane().remove(1);
        _rootPane.getContentPane().repaint();
        _currentProject.dispose();
        _currentProject = null;
        ComponentUtilities.setFrameTitle(_rootPane, Text.getProgramName());
    }

    private void updateFrameTitle() {
        String text;
        if (_currentProject == null) {
            text = Text.getProgramName();
        } else {
            String path = _currentProject.getProjectFilePath();
            if (path == null) {
                text = "<new>  " + Text.getProgramName();
            } else {
                File file = new File(path);
                String name = file.getName();
                int index = name.lastIndexOf('.');
                if (index != -1) {
                    name = name.substring(0, index);
                }
                path = FileUtilities.getAbsolutePath(path);
                text = name + "  " + Text.getProgramName() + "    (" + path + ")";
            }
        }
        ComponentUtilities.setFrameTitle(_rootPane, text);
    }

    public void updateLookAndFeel(Collection windows) {
        Iterator i = windows.iterator();
        while (i.hasNext()) {
            Window window = (Window) i.next();
            SwingUtilities.updateComponentTreeUI(window);
        }
    }

    public void updateUI() {
        SwingUtilities.updateComponentTreeUI(_rootPane);
        Project project = getCurrentProject();
        if (project != null) {
            updateLookAndFeel(project.getOpenWindows());
        }
    }

    public void setExitVMOnApplicationExit(boolean exit) {
        _doExitVM = exit;
    }
}
