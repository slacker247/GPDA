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


import edu.stanford.smi.protege.model.*;
import java.io.*;
import java.util.*;

/**
 * @author Samson Tu
 */
public class ClassHierarchyGenerator {
    private boolean _saveHidden = false;
    private boolean _printInstances;

    public ClassHierarchyGenerator() {
    }

    public void genHierarchy(KnowledgeBase kb, Collection topClses, PrintWriter itsWriter) {
        printHeader(kb, itsWriter);
        if ((topClses == null) || topClses.isEmpty()) {
            Cls root = kb.getRootCls();
            printClses(kb, itsWriter, root.getDirectSubclasses());
        } else {
            printClses(kb, itsWriter, topClses);
        }
        printClosingText(itsWriter);
    }

    public void genHierarchy(KnowledgeBase kb, Collection topClses, PrintWriter itsWriter, boolean saveHidden, boolean printInstances) {
	  _printInstances = printInstances;
        setHidden(saveHidden);
        genHierarchy(kb, topClses, itsWriter);
    }

    private void printClosingText(PrintWriter itsWriter) {
        itsWriter.println("<hr>Generated on " + new Date().toString() + "</body></html>");
    }

    private void printCls(KnowledgeBase kb, PrintWriter itsWriter, Cls cls) {
        String clsRef = cls.getName();
        if (cls.isVisible() || (!cls.isVisible() && _saveHidden)) {
            if (!cls.isSystem())
                clsRef = ClassDocGenerator.hrefToFrame(cls);
            itsWriter.println("<li>");
            itsWriter.println(clsRef);
            itsWriter.println("</li>");
     	     	if (_printInstances)
			printInstances(kb, itsWriter, cls.getDirectInstances());  // display a list of all instances of this class
		Collection classes = cls.getDirectSubclasses();
            if ((classes != null) && (!classes.isEmpty())) {
            	printClses(kb, itsWriter, classes); 	// display all direct subclasses
	  	}
        }
    }

    private void printClses(KnowledgeBase kb, PrintWriter itsWriter, Collection classes) {
        itsWriter.println("<ul>");
        for (Iterator i = classes.iterator(); i.hasNext();) {
            Cls cls = (Cls) i.next();
            printCls(kb, itsWriter, cls);
        }
        itsWriter.println("</ul>");
    }

    private void printInstances(KnowledgeBase kb, PrintWriter itsWriter, Collection instances) {
	  if ((instances==null) || instances.isEmpty())
		return;	// exit if no direct instances of this class
        itsWriter.println("<ul><i>");
	  itsWriter.println("Instances : ");
	  StringBuffer htmlText = new StringBuffer();
        for (Iterator i = instances.iterator(); i.hasNext();) {
            Instance inst = (Instance) i.next();
            htmlText.append(", " + ClassDocGenerator.hrefToFrame(inst));
        }
        itsWriter.println(htmlText.substring(2).toString() + "</i></ul>");
    }

    private void printHeader(KnowledgeBase kb, PrintWriter itsWriter) {
        itsWriter.println("<head>");
        itsWriter.println("<meta http-equiv=\"Content-Type\" content=\"text/html; charset=iso-8859-1\">");
        itsWriter.println("<title>Protege-2000 Class Hierarchy </title>");
        itsWriter.println("</head>");
        itsWriter.println("<body>");
        itsWriter.println("<center><h1> Class Hierarchy for <i>" + kb.getProject().getName() + "</i> Project </h1></center><hr>");

    }

    public void setHidden(boolean saveHidden) {
        this._saveHidden = saveHidden;
    }
}
