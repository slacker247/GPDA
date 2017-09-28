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

package edu.stanford.smi.protegex.queries_tab;

import javax.swing.*;
import java.io.*;
import java.util.*;
import edu.stanford.smi.protege.model.*;

public interface SlotValueWidget{
  public final static int WIDTH = 150;
  public final static int HEIGHT = 40;

    /** Get the possible controls with the display. */
    // public Vector getControls();

    /** Allow any other flexible process */
    public void doTask(String type);

    /** Return the JComponent to show on screen. */
    public JComponent getComponent();

    /** Return an array of Objects that are shown in the widget.
    */
    public Object[] getData();

    /** Return the label of the widget. */
    public String getLabel();

    /** Returns the items currently selected in the JComponent.  For a text box,
             it may be the selected text, or for a list box the selected items in the
             list.  For single valued slots (as compared to multiple valued slots), just
             return an array of size one.
         */
    public String[] getSelectedItems();

    /** Return the selected Object. */
    public Object getSelectedObject();

    /** Every widget is used to represent the value of s specific slot.
         */
    public String getSlotName();

    /** Get the tab. */
    public QueriesTab getTab();

    /** Return the JComponent to hold the data. */
    public JComponent getWidget();

    /** Called after project and kb are set. */
    public void initialized();

    /** whether the seach is done or not */
    public boolean isComplete();

    /** If is a single value slot, set as true. */
    public boolean isSlotSingleValued();

    public boolean isViewEnabled();

    /** get visible */
    public boolean isVisible();

    public void removeListener();

    public void removeMouse();

    /** Performs the search for the specified string with the given search context.
          SetData will be called shortly later with the same array of strings to be
          displayed, so it is not necessary to update the display yet.
         */
    //public Instance[] search(String search, Object searchSpec);

    public Collection search();

    /** This is used to call for the Query settings. */
    public Collection search(Collection instances, Slot slot, String operation, Object obj);

    public void setActionsEnabled(boolean b);

    /** Setup the selected cls */
    public void setCls(Cls cls);

    /** An array of Objects are passed in that should be displayed.  After search
         is called, the array of objects returned will be passed to setData to be
         displayed.  This will also be called when the back and forward buttons are
         pressed to reset the screen from the history.
         */
    public void setData(Object[] data);

    public void setDisplayName(String name);

    /** Setup the instances collection */
    public void setInstances(Collection instances);

    /** Each widget will have a pointer to the project so it can interact
             with the Protege knowledge base.
         */
    public void setProject(Project proj);

    /** Set the displayed selected object. */
    public void setSelectedObject(Object obj);

    /** Setup the search specification. */
    public void setSpecification(SlotSpecification spec);

    public void setViewEnabled(boolean b);

    /** Set visible */
    public void setVisible(boolean visible);
}// endof SlotValueWidget interface
