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

package edu.stanford.smi.RemoteKBTab.toolbox;

import edu.stanford.smi.RemoteKBTab.*;
import javax.swing.*;
import edu.stanford.smi.protege.util.*;
import java.io.*;
import java.util.*;
import java.net.*;

/** This is a class to place helper functions.  Since all of the methods should
 *  be static, it should never be necessary to create an instance of this
 *  class. */
public final class Helper {

    private Helper() {
    }

    /** Each AbstractRemoteKBAction needs to have its RelatinDisplay set. This
     *  is the same for every RelationDisplay. */
    public static void setRelationsDisplay(AbstractRemoteKBAction[] actions, RelationDisplay rd) {
        if (actions != null) {
            for (int i = 0; i < actions.length; i++) {
                actions[i].setRelationDisplay(rd);
            }
	}
    }

    /** LabeledComponents are provided by the Protege API.  They take a
     *  scrollpane and a label and position the label nicely over the
     *  scrollpane. Then each action can be added in the label area above the
     *  scrollpane.  Finally the whole label is then added to the outtermost
     *  JPanel -- itsComp. This series of actions is probably performed for all
     *  RelationDisplay's that have action buttons. */
    public static void createLabeledComponentInPanel(String label, JScrollPane innerPane,
                                                     AbstractRemoteKBAction[] actions, JPanel itsComp) {
        if (label != null || actions != null) {
            LabeledComponent lc = new LabeledComponent(label, innerPane);
	    if (actions != null) {
                for (int i = 0; i < actions.length; i++) {
                    lc.addHeaderButton(actions[i]);
                }
            }
	    itsComp.add(lc);
	} else {
            itsComp.add(innerPane);
        }
    }

    /** Assumes the icon file is a .gif, but that the string passed in does not
     *  have the .gif ending.  The icon needs to be in the same directory as
     *  the Helper.class file or the name needs to be a fully qualified path. */
    public static Icon getIcon(String name) {
        String path = name + ".gif";
        ImageIcon icon = makeIcon(path);
        if (icon == null || icon.getIconWidth() == -1) {
	    System.out.println("Unable to load icon " + path);
	}
	return icon;
    }

    /** Make the icon from the specified path. Return the imageicon. */
    private static ImageIcon makeIcon(String path) {
        ImageIcon icon = null;
        URL url = Helper.class.getResource(path);

        if (url != null) {
            icon = new ImageIcon(url);
            if (icon.getIconWidth() == -1) {
                Log.error("failed to load", Helper.class, "loadIcon", path);
            }
        }
        if (icon == null) {
            icon = null;
        }
	return icon;
    }

    /** Get the values from specified list and return them as a string array. */
    public static String[] listToStringArray(java.util.List list) {
        if (list == null) { return null; }

        String[] result = new String[list.size()];
	ListIterator iterator = list.listIterator();
	int i = 0;
	while(iterator.hasNext()) {
            result[i] = (String)iterator.next();
	    i++;
        }
	return result;
    }

    /** Replace the space with underscore in the specified string. */
    public static String replaceSpaceWithUnderscore(String origstr) {
        if (origstr == null) { return null; }

        int len = origstr.length();
	String newstr = new String("");
	for (int i = 0; i<len;i++) {
            if (origstr.charAt(i)!=' ') {
                newstr = newstr + origstr.charAt(i);
            } else {
                newstr = newstr + "_";
            }
        }
	return newstr;
    }

    /** Deep copy the Vector. */
    public static void copyVec(Vector newvec, Vector oldvec) {
        newvec.clear();
        for (int i = 0; i<oldvec.size(); i++) {
            newvec.addElement(oldvec.elementAt(i));
        }
    }

    /** Get the first Element of earch line. */
    public static String getFirstPart(String resultLine) {
        if (resultLine == null) {
            return null;
        }
        StringTokenizer tokenizer = new StringTokenizer(resultLine," ");
        String tmpStr = new String();
        while(tokenizer.hasMoreTokens()) {
            tmpStr = tokenizer.nextToken();
            return tmpStr.trim();
        }
        return null;
    }
}