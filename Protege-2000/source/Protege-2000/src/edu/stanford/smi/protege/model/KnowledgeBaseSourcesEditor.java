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
import javax.swing.*;
import javax.swing.event.*;
import edu.stanford.smi.protege.util.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public abstract class KnowledgeBaseSourcesEditor extends JComponent implements Validatable {
    private PropertyList _sources;
    private FileField _projectPathField;
    private FileList _includedProjectsList;
    private String _oldProjectPath;
    private boolean _showingProject;

    public KnowledgeBaseSourcesEditor(String projectPath, PropertyList sources) {
        _sources = sources;
        _oldProjectPath = projectPath;
        setLayout(new BorderLayout(10, 10));
        add(createProjectPathField(projectPath), BorderLayout.NORTH);
        createIncludedProjectsList();
    }

    public JComponent createIncludedProjectsList() {
        _includedProjectsList = new FileList("Included Projects", null, ".pprj", "Project Files");
        return _includedProjectsList;
    }

    public JComponent createProjectPathField(String projectPath) {
        _projectPathField = new FileField("Project", projectPath, ".pprj", "Project Files");
        _projectPathField.addChangeListener(
            new ChangeListener() {
                public void stateChanged(ChangeEvent event) {
                    onProjectPathChange(_oldProjectPath, getProjectPath());
                    _oldProjectPath = getProjectPath();
                }
            }
        );
        return _projectPathField;
    }

    public Collection getIncludedProjects() {
        return _includedProjectsList.getPaths();
    }

    public String getProjectPath() {
        String path = _projectPathField.getPath();
        if (path != null && !path.endsWith(".pprj")) {
            path += ".pprj";
        }
        return path;
    }

    public FileField getProjectPathField() {
        return _projectPathField;
    }

    public PropertyList getSources() {
        return _sources;
    }

    protected void onProjectPathChange(String oldName, String newName) {
    }

    protected JComponent getIncludedProjectsList() {
       return _includedProjectsList;
    }

    public void setShowProject(boolean showProject) {
        _showingProject = showProject;
        remove(_projectPathField);
        remove(getIncludedProjectsList());
        if (showProject) {
            add(_projectPathField, BorderLayout.NORTH);
        } else {
            add(getIncludedProjectsList(), BorderLayout.NORTH);
        }
        revalidate();
    }

    public boolean isShowingProject () {
        return _showingProject;
    }
}