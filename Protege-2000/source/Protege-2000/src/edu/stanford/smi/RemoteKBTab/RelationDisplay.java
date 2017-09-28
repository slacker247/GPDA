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

package edu.stanford.smi.RemoteKBTab;

import javax.swing.*;
import java.io.*;
import java.util.*;
import edu.stanford.smi.protege.model.*;

/** Interface that all RelationDisplay's must implement. */
public interface RelationDisplay {

    /** Return the JComponent to show on screen. */
    public JComponent getComponent();

    /** Return the JComponent to hold the data. */
    public JComponent getWidget();

    /** Performs the search for the specified string with the given search
     *  context. SetData will be called shortly later with the same array of
     *  strings to be displayed, so it is not necessary to update the
     *  display yet. */
    public String[] search(String search, Object searchSpec);

    public String[] search(Object searchObj);

    /** An array of strings are passed in that should be displayed.  After
     *  search is called, the array of strings returned will be passed to
     *  setData to be displayed.  This will also be called when the back and
     *  forward buttons are pressed to reset the screen from the history. */
    public void setData(String[] data);

    /** Return an array of strings that are shown in the display. */
    public String[] getData();

    /** Each RelationDisplay likely has an associated slot which is the
     *  relation between the strings from search and the search string. The
     *  slot may be null though, for example, in the UMLS tab there may be a
     *  display that shows the alternative spellings of the search term. */
    public String getSlotName();

    /** Each RelationDisplay will have a pointer to the project so it can
     *  interact with the Protege knowledge base. */
    public void setProject(Project proj);

    /** Set the RemoteKBTab reference. The RemoteKBTab has some useful
     *  functions such as getCurrentInstance, getCurrentClass, getCurrentFrame,
     *  and doSearch. */
    public void setKBTab(RemoteKBTab tab);

    /** Called after project and kb are set. */
    public void initialized();

    /** Returns the items currently selected in the JComponent.  For a text
     *  box, it may be the selected text, or for a list box the selected items
     *  in the list.  For single valued slots (as compared to multiple valued
     *  slots), just return an array of size one. */
    public String[] getSelectedItems();

    /** Return the label of the relation display. */
    public String getLabel();

    /** Set visible */
    public void setVisible(boolean visible);

    /** get visible */
    public boolean isVisible();

    /** If is a single value slot, set as true. */
    public boolean isSlotSingleValued();

    /** Allow any other flexible process */
    public void doTask(String type);

    /** whether the seach is done or not */
    public boolean isComplete();
}