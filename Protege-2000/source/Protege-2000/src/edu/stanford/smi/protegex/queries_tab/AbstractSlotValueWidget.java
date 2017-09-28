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
import edu.stanford.smi.protege.widget.*;
import edu.stanford.smi.protege.util.*;

public abstract class AbstractSlotValueWidget implements SlotValueWidget{

  public final static int FIELDWIDTH = 135;
  public final static int FIELDHEIGHT = 25;

    public Project project;
    protected String slotName;
  protected String label;
  protected boolean visible;

  protected SlotSpecification specification;
  public Cls selection;
  public Slot itsSlot;
  public QueriesTab itsTab;
  protected Collection itsInstances;

  protected String[] constraints;
  public SearchWidget itsWidget;
  protected boolean isViewEnabled = true;

  public AbstractTemplateSlotNumberValidator _validator;

   /** Define table mouse adapter. Single click will show the search result. Double
     click will start a new presearch. */
   protected class SlotValueWidgetMouse extends java.awt.event.MouseAdapter
     {

           public SlotValueWidgetMouse() {

           }

           public void mousePressed(java.awt.event.MouseEvent event)
           {

               if(event.getClickCount() == 2)
               {
            viewObject();
               }
         }


  } // end of TableMouse

    /** Constructor of class AbstractRelationDisplay. */
    public AbstractSlotValueWidget(SearchWidget widget) {
        itsWidget = widget;
        visible = true;
        itsTab = widget.getTab();
    }

    /** flexible task */
    public void doTask(String type) {

    }

    /** Get the reference of current knowledge base in protege. */
    public KnowledgeBase getKB() {
        return project.getKnowledgeBase();
    }

    /** Get the label of RelationDisplay. */
    public String getLabel() {
        if (label != null)
            return label;
        else {
            return new String("");
        }
    }

    /** Get the component which will locate in the north of relation display. */
    public JComponent getNorthComponent() {
        return null;
    }

    /** Get Protege project */
    public Project getProject() {
        return project;
    }

    /** Get the selected items. Return is an string array. */
    public String[] getSelectedItems() {
        return null;
    }

    /** Get the name of the slot for this relation display. */
    public String getSlotName() {
        return slotName;
    }

    public QueriesTab getTab() {
        return itsTab;
    }

    public void initialized() {

    }

    public boolean isComplete() {
        return true;
    }

    /** If the slot is single value, return true. Otherwise, return false. */
    public boolean isSlotSingleValued() {
        return true;
    }

    public boolean isViewEnabled() {
        return isViewEnabled;
    }

    /** get visible */
    public boolean isVisible() {
        return visible;
    }

    public void removeListener() {
    }

    public void removeMouse() {
    }

    /** For the formed search result, change the format. */
    public Collection search() {
        return null;
    }

    /** Enable/Disable the icons. */
    public void setActionsEnabled(boolean b) {

    }

    /** Setup the selected cls */
    public void setCls(Cls cls) {
        selection = cls;
    }

    public void setDisplayName(String name) {
    }

    /** Setup the instances collection */
    public void setInstances(Collection instances) {
        itsInstances = instances;
    }

    /** Set the Protege project. */
    public void setProject(Project proj) {
        project = proj;
    }

    /** Set the name of the slot for this relation display. */
    public void setSlotName(String n) {
        slotName = n;
    }

    /** Setup the search specification. */
    public void setSpecification(SlotSpecification spec) {
        specification = spec;
    }

    public void setViewEnabled(boolean b) {
        isViewEnabled = b;
    }

    /** Set visible */
    public void setVisible(boolean v) {
        visible = v;
    }

    public void viewObject() {

    }
}
