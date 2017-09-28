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

/** AddClassAction is used to add selected items as classes in protege knowledge base. */
public class AddClassAction extends AbstractAction implements AbstractRemoteKBAction {
  RelationDisplay display;
	KnowledgeBase kb;
	RemoteKBTab tab;
	String slotName;
	Slot slot;

/** Constructor. */
	public AddClassAction(RemoteKBTab tab) {
		this("Add Selected Value as a class", tab, Icons.getClsIcon());

	}

/** Constructor with tooltip, RemoteKBTab, and icon. */
	public AddClassAction(String tooltip, RemoteKBTab tab, Icon i){
		super(tooltip, i);
		this.tab = tab;
		kb = tab.getProject().getKnowledgeBase();
	}

/**  All AbstractRemoteKBAction classes must have the method
	 setRelationDisplay, so the action can be associated with the
	 RelationDisplay. */
	public void setRelationDisplay(RelationDisplay rd){
		display = rd;
		slotName = rd.getSlotName();
		if (slotName == null) return;
		slot = createSlot(slotName);
    slot.setAllowsMultipleValues(!rd.isSlotSingleValued());
	}

/** Perform the add class action. Add selected items as classes.Set selected string to
    the 'Name' slot of the new class. */
	public void actionPerformed(ActionEvent e){

    //System.out.println("add the classes from addclassaction in toolbox");
		Cls currentSel = tab.getSelectedCls();
		if (currentSel == null) {
			return;
		}
		String[] values = display.getSelectedItems();
		String value;
		if (values == null) {
			return;
		}

		Collection parentcls = new ArrayList();
		parentcls.add(currentSel);

		for ( int i =0; i< values.length; i++) {
       Cls cls = kb.createCls(values[i],parentcls);
			 // Cls cls = kb.createCls(null ,parentcls);
			 //ModelUtilities.setOwnSlotValue(cls, ":NAME", values[i]);
       ModelUtilities.setOwnSlotValue(cls, "name", values[i]);

    }
  }


/** If specified slot does not exist, create it. */
	protected Slot createSlot(String name){
		Slot check = kb.getSlot(name);
		if (check == null){
			check = kb.createSlot(name);
      check.setAllowsMultipleValues(!display.isSlotSingleValued());
		}

		return check;
	}
}