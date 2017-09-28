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

import javax.swing.*;
import edu.stanford.smi.RemoteKBTab.*;
import edu.stanford.smi.protege.resource.*;
import edu.stanford.smi.protege.model.*;
import java.awt.event.*;
import java.util.*;

/** AddClassAction is used to add selected items as classes in protege
 *  knowledge base. */
public class AddClassWithChildrenAction extends AbstractAction
                                        implements AbstractRemoteKBAction {
    RelationDisplay display;
    KnowledgeBase kb;
    RemoteKBTab tab;
    String slotName;
    Slot slot;
    RelationDisplay[] displays;
    private int addstyle;

    public AddClassWithChildrenAction(RemoteKBTab tab) {
        this("add selected item as a class", tab, Icons.getClsIcon());
    }

    public AddClassWithChildrenAction(String tooltip, RemoteKBTab tab, Icon i){
        super(tooltip, i);
        this.tab = tab;
        kb = tab.getProject().getKnowledgeBase();
    }

    /**  All AbstractRemoteKBAction classes must have the method
     *   setRelationDisplay, so the action can be associated with the
     *   RelationDisplay. */
    public void setRelationDisplay(RelationDisplay rd){
        display = rd;
        slotName = rd.getSlotName();
        if (slotName == null) { return; }
        slot = createSlot(slotName);
        slot.setAllowsMultipleValues(!rd.isSlotSingleValued());
    }

    /** Perform the add class action. Add selected items as classes. Set
     *  selected string to the 'Name' slot of the new class. */
    public void actionPerformed(ActionEvent e) {

        // 04-16-02, jlv - Since the remote kb tab is something that other tabs
        // in protege implement, there really shouldn't be any text here that's
        // specific to the umls tab.  However, the only other tab that implements
        // the remote kb tab is the wordnet tab which doesn't use this code.  I
        // also don't anticipate many people trying to implement the remote kb
        // interface since it seems overly complicated and restricts the developer
        // to a particular user interface.  So, the dialogs that we pop up here
        // are a fix for behavior in the umls tab and will remain here unless
        // someone requests different behavior in the future.

        String[] values = display.getSelectedItems();
        if (values.length == 0) {
            JOptionPane.showMessageDialog(tab, "Please select an item in the UMLS Narrow Tree",
                                          "UMLS Tab", JOptionPane.INFORMATION_MESSAGE);
        } else {
            Cls currentSel = tab.getSelectedCls();
            if (currentSel == null) {
                JOptionPane.showMessageDialog(tab, "Please select an class in the Classes tab", "UMLS Tab", JOptionPane.INFORMATION_MESSAGE);
            } else {
                display.doTask("class");
            }
        }
    }

    /** If specified slot does not exist, create it. */
    protected Slot createSlot(String name) {
        Slot check = kb.getSlot(name);
        if (check == null){
            check = kb.createSlot(name);
            check.setAllowsMultipleValues(!display.isSlotSingleValued());
        }
        return check;
    }
}