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
import java.awt.*;
import java.util.*;
import java.awt.event.*;

/* Provides a default implementation of a JList.
	 To use this class, simply override it and create a new search function.
 */
 public abstract class ListRelationDisplay extends AbstractRelationDisplay {
	JPanel itsComp;
	JList itsList;
	String[] data;

/** Constructor. */
	public ListRelationDisplay() {
		createComponents(null, null);

	}

/** Constructor with label, slot, and actions. */
	public ListRelationDisplay(String label, String slot, AbstractRemoteKBAction[] actions){
		setSlotName(slot);
		Helper.setRelationsDisplay(actions, this);
		createComponents(label, actions);
		this.label = label;

	}

/** Create components in this relation display. */
	private void createComponents(String label, AbstractRemoteKBAction[] actions){
		itsList = new JList();

		JScrollPane scroll = new JScrollPane(itsList);
		scroll.setPreferredSize(new Dimension(150, 200));

		itsComp = new JPanel();
		Helper.createLabeledComponentInPanel(label, scroll, actions, itsComp);
	}

/** Return the relation display component. */
	public JComponent getComponent(){
		return itsComp;
	}

/** Return the embedded widget in the relation display, the list. */
	public JComponent getWidget() {
		return itsList;
	}

/** Set the values of string array to the list. */
	public void setData(String[] data){
		itsList.clearSelection();
		itsList.removeAll();
		if(data != null){
			this.data = new String[data.length];
			for(int i = 0; i < data.length; i++){
				this.data[i] = data[i];
			}
		}
		else{
			this.data = new String[0];
		}
		itsList.setListData(this.data);
		itsList.repaint();
	}

/** Get the data from the list and return it as a string array. */
	public String[] getData() {
		return data;
  }

/** Get the values from specified list and return them as a string array. */
  private String[] listToStringArray(java.util.List list){
	 if (list == null) return null;
   String[] result = new String[list.size()];
	 ListIterator iterator = list.listIterator();
	 int i = 0;
	 while(iterator.hasNext()){
		result[i] = (String)iterator.next();
		i++;
	 }
	 return result;
	}

/** Get the selected items from the list and return them as a string array. */
	public String[] getSelectedItems(){
   	Object[] selected = itsList.getSelectedValues();
	  if (selected == null|| selected.length < 1) return null;

		String[] strings = new String[selected.length];
		for(int i = 0; i < strings.length;i++){
			strings[i] = selected[i].toString();
		}

		return strings;
	}

/** Set the slot not a single value slot. */
  public boolean isSlotSingleValued(){
   	return false;
	}



}//end of ListRelationDisplay