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

package edu.stanford.smi.protegex.htmldoc;


import java.util.*;
import java.awt.*;
import java.io.*;
import javax.swing.*;
import java.awt.event.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.*;
import edu.stanford.smi.protege.util.*;
import edu.stanford.smi.protege.ui.*;
import edu.stanford.smi.protege.widget.*;

/**
 * @author Samson Tu
 */
public class ProtegeGenClassHierarchy {

  private boolean _invokedStandalone = true;
  private static ClassHierarchyGenerator _treeGen;

    public ProtegeGenClassHierarchy() {
    }

    public static void generateDocs(KnowledgeBase kb, Collection topClses, boolean saveHidden,
        String indexPage, String outputDir, boolean printInstances)
    {
        //***********First generate documentation page for classes
        ClassDocGenerator classDocGen = new ClassDocGenerator();
        try {
            classDocGen.genClsesDoc(kb, topClses, saveHidden, indexPage, outputDir, printInstances);
        } catch (Exception e) {
            Log.exception(e, ProtegeGenClassHierarchy.class, "generateDocs");
        }

        //********** Generate hierarchy of classes  *************
        File treeFile = new File(outputDir, indexPage);
        ;
        PrintWriter itsWriter = null;
        try {
            itsWriter = new PrintWriter(new FileWriter(treeFile), true);
        } catch (Exception e) {
            Log.exception(e, ProtegeGenClassHierarchy.class, "generateDocs");
        }

        ClassHierarchyGenerator classTreeGen = new ClassHierarchyGenerator();

        classTreeGen.genHierarchy(kb, topClses, itsWriter, saveHidden, printInstances);
    }

    public static void main(String[] args) {
        ProtegeGenClassHierarchy genTree = new ProtegeGenClassHierarchy();

        if (args.length != 1) {
            System.out.println("ProtegeGenClassHierarchy needs initialization file as an argument");
            return;
        }
        String initfile = args[0];
        System.out.println("ProtegeGenClassHierarchy:initfile: " + initfile);
        Properties settings = new Properties();
        try {
            FileInputStream sf = new FileInputStream(initfile);
            settings.load(sf);
        } catch (Exception ex) {
            System.out.println("Exception during loading initialization file");
            System.out.println(ex.toString());
        }
        String kbURL = settings.getProperty("KBURL", "");
        String outputDir = settings.getProperty("OUTPUTDIR", "");
        String indexPage = settings.getProperty("FILENAME", "index.html");
        String saveHiddenString = settings.getProperty("SAVEHIDDEN", "");
        String savePrintInstancesString = settings.getProperty("SAVEPRINTINSTANCES", "");
        String topClasses = settings.getProperty("TOPCLASSES", null);

        KnowledgeBase kb = null;
        Project project = null;
        Collection error_messages = new ArrayList();
        boolean saveHidden = false;
        if (saveHiddenString.equals("true"))
            saveHidden = true;
        else
            saveHidden = false;
        boolean printInstances = false;
        if (savePrintInstancesString.equals("true"))
            printInstances = true;
        else
            printInstances = false;

        if (outputDir != null) {
            try {
                project = Project.loadProjectFromFile(kbURL, error_messages);
                kb = project.getKnowledgeBase();
            } catch (Exception e) {
                System.out.println("Error loading knowledge base");
                e.printStackTrace();
                System.exit(1);
            }

            //***** Get toplevel classes

            Collection topClses = new ArrayList();
            Cls topCls = null;
            int stringIndex = 0;
            boolean hasNext = true;

            if (topClasses != null) {
                while (hasNext) {
                    int endIndex = topClasses.indexOf(",", stringIndex);
                    if (endIndex < 0) {
                        topCls = kb.getCls(topClasses.substring(stringIndex, topClasses.length()));
                        if (topCls != null)
                            topClses.add(topCls);
                        else
                            System.out.println(topClasses.substring(stringIndex, topClasses.length()) + " is not the name of a class");
                        hasNext = false;
                    } else {
                        topCls = kb.getCls(topClasses.substring(stringIndex, endIndex));
                        if (topCls != null)
                            topClses.add(topCls);
                        else
                            System.out.println(topClasses.substring(stringIndex, topClasses.length()) + " is not the name of a class");
                        stringIndex = endIndex + 1;
                    }
                }
            }

            generateDocs(kb, topClses, saveHidden, indexPage, outputDir, printInstances);
        }
    }
}
