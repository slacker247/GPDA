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

package edu.stanford.smi.protege.storage.clips;


import java.awt.*;
import java.awt.event.*;
import java.io.*;
import javax.swing.*;
import javax.swing.event.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.resource.*;
import edu.stanford.smi.protege.util.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class FileSourcesPanel extends KnowledgeBaseSourcesEditor {
    private FileField _clsesField;
    private FileField _instancesField;

    public FileSourcesPanel(String projectPath, PropertyList sources) {
        super(projectPath, sources);
        JPanel panel = new JPanel();
        panel.setLayout(new GridLayout(2, 1, 10, 10));
        panel.add(createClsesField());
        panel.add(createInstancesField());
        add(panel);
    }

    private String constructName(String projectPath, String extension) {
        String name = null;
        if (projectPath != null) {
            int index = projectPath.indexOf(".");
            if (index != -1) {
                name = projectPath.substring(0, index) + extension;
            }
        }
        return name;
    }

    private JComponent createClsesField() {
        String name = ClipsKnowledgeBaseFactory.getClsesSourceFile(getSources());
        if (name == null) {
            name = constructName(getProjectPath(), ".pont");
        }
        _clsesField = new FileField("Classes file name", name, ".pont", "Ontology");
        return _clsesField;
    }

    private JComponent createInstancesField() {
        String name = ClipsKnowledgeBaseFactory.getInstancesSourceFile(getSources());
        if (name == null) {
            name = constructName(getProjectPath(), ".pins");
        }
        _instancesField = new FileField("Instances file name", name, ".pins", "Instances");
        return _instancesField;
    }

    public String getProjectPath() {
        String path = super.getProjectPath();
        if (path == null && _clsesField != null) {
            String text = _clsesField.getPath();
            if (text != null) {
                text = text.substring(0, text.length() - 4);
                text += "pprj";
                path = text;
            }
        }
        return path;
    }

    private boolean hasValidValue(FileField field) {
        boolean hasValidValue;
        String value = field.getPath();
        if (value == null || value.length() == 0) {
            hasValidValue = false;
        } else {
            // other tests???
            hasValidValue = true;
        }
        return hasValidValue;
    }

    public void onProjectPathChange(String oldPath, String newPath) {
        if (newPath != null) {
            updatePath(_clsesField, ".pont");
            updatePath(_instancesField, ".pins");
        }
    }

    public void saveContents() {
        String clsesFileName = _clsesField.getPath();
        String instancesFileName = _instancesField.getPath();
        ClipsKnowledgeBaseFactory.setSourceFiles(getSources(), clsesFileName, instancesFileName);
    }

    private void updatePath(FileField field, String ext) {
        File projectFile = new File(getProjectPath());
        String projectName = projectFile.getName();
        String projectDir = projectFile.getParent();
        int index = projectName.indexOf(".");
        if (index != -1) {
            projectName = projectName.substring(0, index);
        }
        String newPath = projectName + ext;
        field.setPath(newPath);
    }

    public boolean validateContents() {
        boolean isComplete = hasValidValue(_clsesField) && hasValidValue(_instancesField);
        if (!isComplete) {
            // TODO
            isComplete = true;
        }
        return isComplete;
    }
}
