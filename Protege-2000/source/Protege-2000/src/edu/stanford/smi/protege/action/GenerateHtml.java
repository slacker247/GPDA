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

package edu.stanford.smi.protege.action;

import java.awt.event.*;
import java.io.*;
import java.util.*;
import javax.swing.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.resource.*;
import edu.stanford.smi.protege.ui.*;
import edu.stanford.smi.protege.util.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class GenerateHtml extends ProjectAction {

    public GenerateHtml() {
        super("Generate HTML...", null);
    }

    public void actionPerformed(ActionEvent event) {
        Project project = getProjectManager().getCurrentProject();
        if (project != null) {
            KnowledgeBase kb = project.getKnowledgeBase();
            GenerateHtmlOptionsPanel panel = new GenerateHtmlOptionsPanel(kb);
            JComponent parent = getProjectManager().getMainPanel();
            int choice = ModalDialog.showDialog(parent, panel, "Generate HTML Options", ModalDialog.MODE_OK_CANCEL);
            if (choice == ModalDialog.OPTION_OK) {
                Collection clses = panel.getRootClses();
                String output = panel.getOutputDirectory();
                boolean includeInstances = panel.getIncludeInstances();
                generateHtml(kb, clses, output, includeInstances);
            }
        }
    }

    private void generateHtml(KnowledgeBase kb, Collection clses,
            String outputDir, boolean includeInstances) {
        WaitCursor cursor = new WaitCursor(getProjectManager().getMainPanel());
        cursor.show();
        try {
            File file = new File(outputDir);
            if (!file.exists()) {
                file.mkdirs();
            }
            edu.stanford.smi.protegex.htmldoc.ProtegeGenClassHierarchy.
                    generateDocs(kb, clses, true, "index.html", outputDir, includeInstances);
        } finally {
            cursor.hide();
        }
    }
}
