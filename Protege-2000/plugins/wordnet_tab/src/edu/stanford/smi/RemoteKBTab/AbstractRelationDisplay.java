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


/** AbstractRelationDisplay provides a default implementation of the
 *  RelationDisplay interface, except for getComponent, search, and setData.
 *  Most RelationDisplay should probably inherit from this abstract class. */
public abstract class AbstractRelationDisplay implements RelationDisplay {

    protected Project project;
    public RemoteKBTab tab;
    protected String slotName;
    protected String label;
    protected boolean visible;

    public AbstractRelationDisplay() {
        visible = true;
    }

    public void initialized(){
    }

    /** Set the Protege project. */
    public void setProject(Project proj){
        project = proj;
    }

    /** Get the label of RelationDisplay. */
    public String getLabel() {
        if (label != null) {
            return label;
        } else {
            return new String("");
        }
    }

    /** Set remoteKB Tab reference. */
    public void setKBTab(RemoteKBTab tab) {
        this.tab = tab;
    }

    /** Get RemoteKB Tab reference. */
    public RemoteKBTab getTab() {
        return tab;
    }

    /** Get Protege project */
    public Project getProject() {
        return project;
    }

    /** Get the reference of current knowledge base in protege. */
    public KnowledgeBase getKB() {
        return project.getKnowledgeBase();
    }

    /** Get the name of the slot for this relation display. */
    public String getSlotName() {
        return slotName;
    }

    /** Set the name of the slot for this relation display. */
    public void setSlotName(String n) {
        slotName = n;
    }

    /** Get the selected items. Return is an string array. */
    public String[] getSelectedItems() {
        return null;
    }

    /** Set visible */
    public void setVisible(boolean v) {
        visible = v;
    }

    /** get visible */
    public boolean isVisible() {
        return visible;
    }

    /** Get the component which will locate in the north of relation display. */
    public JComponent getNorthComponent() {
        return null;
    }

    /** If the slot is single value, return true. Otherwise, return false. */
    public boolean isSlotSingleValued() {
        return true;
    }

    /** For the formed search result, change the format. */
    public String[] search(Object searchObj) {
        return null;
    }

    /** flexible task */
    public void doTask(String type) {
    }

    public boolean isComplete() {
        return true;
    }
}