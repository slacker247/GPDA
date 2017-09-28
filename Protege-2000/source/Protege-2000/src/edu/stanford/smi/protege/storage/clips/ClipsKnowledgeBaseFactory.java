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

import java.io.*;
import java.net.*;
import java.util.*;
import javax.swing.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.util.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class ClipsKnowledgeBaseFactory implements KnowledgeBaseFactory {
    private final static String CLSES_FILE_NAME = "classes_file_name";
    private final static String INSTANCES_FILE_NAME = "instances_file_name";

    public ClipsKnowledgeBaseFactory() {
    }

    public KnowledgeBase createKnowledgeBase(Collection errors) {
        return new DefaultKnowledgeBase(this);
    }

    public KnowledgeBaseSourcesEditor createKnowledgeBaseSourcesEditor(String projectName, PropertyList list) {
        return new FileSourcesPanel(projectName, list);
    }

    private Reader createReader(String name, Collection errors) {
        Reader reader = FileUtilities.getReader(name);
        if (reader == null && name != null) {
            errors.add(new Exception("Unable to read file with name " + name));
        }
        return reader;
    }

    private Writer createWriter(String name, Collection errors) {
        Writer writer = FileUtilities.getWriter(name);
        if (writer == null && name != null) {
            errors.add(new Exception("Unable to write file with name " + name));
        }
        return writer;
    }

    public static String getClsesSourceFile(PropertyList sources) {
        return sources.getString(CLSES_FILE_NAME);
    }

    public String getDescription() {
        return "Standard Text Files";
    }

    public static String getInstancesSourceFile(PropertyList sources) {
        return sources.getString(INSTANCES_FILE_NAME);
    }

    public String getProjectFilePath() {
        return null;
    }

    public void includeKnowledgeBase(KnowledgeBase kb, PropertyList sources, Collection errors) {
        loadKnowledgeBase(kb, sources, true, errors);
    }

    public boolean isComplete(PropertyList sources) {
        // If either file is not set we now generate it from the project name.
        // return getClsesSourceFile(sources) != null && getInstancesSourceFile(sources) != null;
        return true;
    }

    public void loadKnowledgeBase(KnowledgeBase kb, PropertyList sources, Collection errors) {
        loadKnowledgeBase(kb, sources, false, errors);
    }

    private void loadKnowledgeBase(KnowledgeBase kb, PropertyList sources, boolean isInclude, Collection errors) {
        String clsesName = getClsesSourceFile(sources);
        String instancesName = getInstancesSourceFile(sources);
        loadKnowledgeBase(kb, clsesName, instancesName, isInclude, errors);
    }

    private void loadKnowledgeBase(KnowledgeBase kb, Reader clsesReader, Reader instancesReader, boolean isInclude, Collection errors) {
        // TODO should probably have separate cls and instances parsers
        kb.setLoading(true);
        if (clsesReader != null) {
            Parser delegate = new Parser(clsesReader);
            delegate.loadClses(kb, isInclude, errors);
        }
        if (instancesReader != null) {
            Parser delegate = new Parser(instancesReader);
            delegate.loadInstances(kb, isInclude, errors);
        }
        kb.setLoading(false);
    }

    private void loadKnowledgeBase(
        KnowledgeBase kb,
        String clsesName,
        String instancesName,
        boolean isInclude,
        Collection errors) {
        Reader clsesReader = null;
        Reader instancesReader = null;
        try {
            clsesReader = createReader(clsesName, errors);
            instancesReader = createReader(instancesName, errors);
            loadKnowledgeBase(kb, clsesReader, instancesReader, isInclude, errors);
        } finally {
            SystemUtilities.close(clsesReader);
            SystemUtilities.close(instancesReader);
        }
    }

    public KnowledgeBase loadKnowledgeBase(Reader clsesReader, Reader instancesReader, Collection errors) {
        KnowledgeBase kb = createKnowledgeBase(errors);
        loadKnowledgeBase(kb, clsesReader, instancesReader, false, errors);
        return kb;
    }

    public KnowledgeBase loadKnowledgeBase(String clsesFileName, String instancesFileName, Collection errors) {
        KnowledgeBase kb = createKnowledgeBase(errors);
        loadKnowledgeBase(kb, clsesFileName, instancesFileName, false, errors);
        return kb;
    }

    private static String pathToName(String path) {
        return new File(path).getName();
    }

    public void saveKnowledgeBase(KnowledgeBase kb, PropertyList sources, Collection errors) {
        boolean changed = false;
        String projectBaseName = kb.getProject().getName();
        String clsesName = getClsesSourceFile(sources);
        if (clsesName == null) {
            clsesName = projectBaseName + ".pont";
            changed = true;
        }
        String instancesName = getInstancesSourceFile(sources);
        if (instancesName == null) {
            instancesName = projectBaseName + ".pins";
            changed = true;
        }
        if (changed) {
            setSourceFiles(sources, clsesName, instancesName);
        }
        saveKnowledgeBase(kb, clsesName, instancesName, errors);
    }

    public void saveKnowledgeBase(KnowledgeBase kb, Writer clsesWriter, Writer instancesWriter, Collection errors) {
        if (clsesWriter != null) {
            ClsStorer clsStorer = new ClsStorer(clsesWriter);
            clsStorer.storeClses(kb, errors);
        }
        if (instancesWriter != null) {
            InstanceStorer instanceStorer = new InstanceStorer(instancesWriter);
            instanceStorer.storeInstances(kb, errors);
        }
    }

    public void saveKnowledgeBase(KnowledgeBase kb, String clsesFilename, String instancesFilename, Collection errors) {
        Project project = kb.getProject();
        File projectDir = (project == null) ? (File) null : project.getProjectDirectory();
        Writer pontWriter = null;
        Writer pinsWriter = null;
        try {
            File pontFile = FileUtilities.createTempFile(projectDir, clsesFilename);
            File pinsFile = FileUtilities.createTempFile(projectDir, instancesFilename);
            pontWriter = FileUtilities.createWriter(pontFile);
            pinsWriter = FileUtilities.createWriter(pinsFile);
            saveKnowledgeBase(kb, pontWriter, pinsWriter, errors);
            SystemUtilities.close(pontWriter);
            SystemUtilities.close(pinsWriter);
            if (errors.isEmpty()) {
                FileUtilities.makeTempFilePermanent(pontFile);
                FileUtilities.makeTempFilePermanent(pinsFile);
            }
        } catch (Exception e) {
            errors.add(e);
            SystemUtilities.close(pontWriter);
            SystemUtilities.close(pinsWriter);
        }
    }

    public static void setSourceFiles(PropertyList sources, String clsesFileName, String instancesFileName) {
        sources.setString(CLSES_FILE_NAME, pathToName(clsesFileName));
        sources.setString(INSTANCES_FILE_NAME, pathToName(instancesFileName));
    }

    public String toString() {
        return "ClipsKnowledgeBaseFactory";
    }
}
