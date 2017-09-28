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
import java.io.*;
import java.awt.*;
import edu.stanford.smi.protege.util.*;
import edu.stanford.smi.RemoteKBTab.*;

/** Creates a JEditorPane to display the text.  Set data takes the array of
	 strings passed to it and concatenates them together.
	 To use this class, simply override it and create a new search function.
 */
public abstract class TextEditorRelationDisplay extends AbstractRelationDisplay{

	JEditorPane itsField;
	JPanel itsComp;

/** Constructor. */
	public TextEditorRelationDisplay() {
		createComponents(null, null);
	}

  /** Constructor with label, slot, and actions. */
	public TextEditorRelationDisplay(String label, String slot, AbstractRemoteKBAction[] actions){
		setSlotName(slot);
		Helper.setRelationsDisplay(actions, this);
		createComponents(label, actions);
    this.label = label;
	}

/** Create an Editorpane. */
	private JEditorPane createTextEditor(){
		JEditorPane editor = new JEditorPane();
		editor.setEditable(false);
    editor.setEditorKit(editor.getEditorKitForContentType("text/html"));
		return editor;
	}

/** Create components in this relation display. */
	private void createComponents(String label, AbstractRemoteKBAction[] actions){
		itsField = createTextEditor();
		JScrollPane scroll = new JScrollPane(itsField);
		scroll.setPreferredSize(new Dimension(150, 40));

		itsComp = new JPanel();
		Helper.createLabeledComponentInPanel(label, scroll, actions, itsComp);
	}

/** Return the relation display component. */
	public JComponent getComponent(){
		return itsComp;
	}

/** Return the embedded widget in the relation display, the JEditorPane. */
  public JComponent getWidget() {
    return itsField;
  }

/** Set the specified string array to the editor pane. */
	public void setData(String[] data){
		if (data == null){
			itsField.setText("");
      itsField.repaint();
			return;
		}
		StringBuffer result = new StringBuffer();

		for(int i = 0; i < data.length; i++){
			result.append(data[i]);
		}

		itsField.setText(result.toString().trim());
    itsField.repaint();

	}

/** Get the data from the editor pane and return it as a string array. */
  public String[] getData() {
    String[] itsData = new String[1];
    itsData[0] = new String(itsField.getText());
    return itsData;
  }

/** Get the selected string in editor pane and return them as a string array. */
	public String[] getSelectedItems(){
		String[] text = new String[1];
		text[0] = itsField.getSelectedText();
		if (text[0] == null) text[0] = itsField.getText();
		return text;
	}

/** Set the slot  a single value slot. */
  public boolean isSlotSingleValued(){
   	return true;
	}



}