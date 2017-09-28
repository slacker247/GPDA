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

/** One detail instruction is organized in steps. */
package edu.stanford.smi.RemoteKBTab;

import javax.swing.*;

import edu.stanford.smi.RemoteKBTab.*;
import edu.stanford.smi.RemoteKBTab.toolbox.*;
import WNInterface.*;
import java.util.*;
import javax.swing.*;
import java.awt.*;
import edu.stanford.smi.protege.util.*;

/** One implementation for RemoteKB Tab for WordNet */
public class WordNetTab extends RemoteKBTab {

	private WNInterface wn;
	private JComboBox posCombo;

	public static final int ALL = 0;

  /** Constructor. */
	public WordNetTab() {
  	wn = new WNInterface();

	}

  /**  Get WordNet tab relationTable. */
  public RelationTable getRelationTable() {
		return new WordNetTable(this, wn);
  }

  /** Specify name, type and related actions for each relation display. */
	protected RelationDisplay[] createRelationDisplays(){
		AbstractRelationDisplay[] temp = new AbstractRelationDisplay[4];

		temp[0] = new WNDefinitions(wn, this);
		temp[1] = new WNSynonyms(wn, this);
		temp[2] = new WNChildren(wn, this);
		temp[3] = new WNMorph(wn, this);
		return temp;

	}

  /** Specify root class name. */
	protected String getClassName(){
		return "WordNet";
	}

  /** Specify add slot action. */
	public AbstractRemoteKBAction[] createActions(){
		 AbstractRemoteKBAction[] list = new AbstractRemoteKBAction[1];
		 list[0] = new AddOwnSlotAction(this);
		 return list;

		}

  /** Set up WordNet tab which include tab name and icon. */
	public void setup(){
		setLabel ("WordNet");
		Icon icon = Helper.getIcon("wnsmall");
		if (icon == null){
			System.out.println("failed to create wordnet icon");
		}
		else setIcon(icon);
	}

  /** Get search specification object. */
	public Object getSearchSpecification(){
		return new Integer(posCombo.getSelectedIndex());
	}

  /** Get setting value of combobox in search specification. */
	public String getPOS(){
		return (String) posCombo.getSelectedItem();
	}

  /** Get index of setting in combobox. */
	public int getPOSCode(){
		return posCombo.getSelectedIndex();
	}

  /** get unique name, which include search string and specification. */
	protected String getUniqueName(String targetStr){
		return targetStr + ":"+getPOS();
	}

  /** Restore specification history. */
	protected void restoreSpecHistory(Object item){
		posCombo.setSelectedIndex(((Integer) item).intValue());
	}

  /** Get search specification panel. */
	protected JPanel getSearchSpecPanel(){
		String[] stringArray = {"ALL", "NOUN", "VERB", "ADJ", "ADV"};
		posCombo = new JComboBox(stringArray);
		posCombo.setPreferredSize(new Dimension(80,20));
		LabeledComponent lc = new LabeledComponent("Part of Speech", posCombo);
		JPanel panel = new JPanel();
		panel.add(lc);
		return panel;
	}

}// end of WordNetTab class

/** WordNet synonym relationdisplay is inherited from list RelationDisplay. */
class WNSynonyms extends ListRelationDisplay{

	WNInterface wn;

  /** Constructor. */
	public WNSynonyms(WNInterface w, WordNetTab tab){
		super("Synonyms", "synonym", tab.createActions());
		wn = w;
	}

  /** Search function is used to get data in the format of string array. */
	public String[] search(String search, Object data){
		if (search == null) return null;
		search = search.replace(' ', '_');
		int pos = ((Integer)data).intValue();
		String[] info;
		if (pos == WordNetTab.ALL) info = wn.getSimilarWords(search);
		else info = wn.getSimilarWords(search, pos);
		if (info == null) return null;
		Vector results = new Vector();
		for (int i = 0; i < info.length; i++){
			String oneString = info[i];
			if(!results.contains(oneString))
					results.add(oneString);
		}

		return Helper.listToStringArray(results);
	 }

}// end of WNSynonyms

/** WordNet children relationdisplay is inherited from list RelationDisplay. */
class WNChildren  extends ListRelationDisplay{
	WNInterface wn;

  /** Constructor. */
	public WNChildren(WNInterface w,WordNetTab tab){
		super("Children", "child-of", tab.createActions());
		wn = w;
	}

	/** ignores the search specification.  WordNet only has
	 * support for the children relation on nouns and verbs
	 */
	public String[] search(String search, Object data){
		search = search.replace(' ', '_');
		return wn.getDirectChildren(search);
	}

} //end of WNChildren

/** WordNet definition relationdisplay is inherited from list RelationDisplay. */
class WNDefinitions extends ListRelationDisplay{

	WNInterface wn;

  /** Constructor. */
	public WNDefinitions(WNInterface w, WordNetTab tab){
		super("Definitions", "definition", tab.createActions());
		wn = w;
	}

  /** Search function is used to get data in the format of string array. */
	public String[] search(String search, Object data){
		search = search.replace(' ', '_');
		int pos = ((Integer)data).intValue();
		String[] wordDef;

		if (pos == WordNetTab.ALL)wordDef = wn.getWordsAndDefinitions(search);
		else wordDef = wn.getDefinitions(search, pos);
		if (wordDef == null) return null;

		String[] def = new String[wordDef.length];
		for(int i = 0; i < wordDef.length; i++){
			def[i] = definitionSubstring(wordDef[i]);
		}
		return def;
	}

	/*word1, word2,.. : definition for this sense
	 * returns the substring after the colon
	 */
	private String definitionSubstring(String word){
		if (word == null) return null;
		int pos = word.indexOf(":");
		int start;
		if (pos < 0) start = 1;				/* case where the definition begins and ends with '(' */
		else start = pos + 2;

		return WNInterface.removeLeadingSpaces(word.substring(start, word.length() - 1));

	}

}// end of WNDefinitions

/** WordNet Morphological relationdisplay is inherited from list RelationDisplay. */
class WNMorph extends ListRelationDisplay{

	WNInterface wn;

  /** Constructor. */
	public WNMorph(WNInterface w, WordNetTab tab){
		super("Morphological forms", "morphological_form", tab.createActions());
		wn = w;
	}

  /** Search function is used to get data in the format of string array. */
	public String[] search(String search, Object data){
		search = search.replace(' ', '_');
		int pos = ((Integer)data).intValue();
		if (pos != WordNetTab.ALL) return wn.getMorphForms(search, ((Integer)data).intValue());
		else return wn.getAllMorphForms(search);
	}

}// end of WNMorph

 /** Relation Table class for WordNet Tab. */
class WordNetTable extends AbstractRelationTable{
	String currentWord = null;
	WordNetTab tab = null;
	WNInterface wn = null;

  /** Constructor. */
	public WordNetTable(WordNetTab wntab, WNInterface wnInterface){
		super(new String[]{"Superstring", "Part of Speech"}, new Vector(), new Vector());
		tab = wntab;
		wn = wnInterface;

	}

  /** get the searchResult content. */
	 public String getSearchResultText() {
		return currentWord;
	 }

	 /** get the current selected Obj in the table. */
	 public Object getSearchContext(String term) {
			return null;
	 }

   /** Search function is used to get data in the format of string array. */
	 public void search(String targetStr) {
			targetStr = targetStr.replace(' ', '_');
			String[][] superStrings = wn.searchBySubstring(targetStr);
			if (superStrings == null){
				Vector empty = new Vector();
				setModelVector(empty);
				setTableVecObj(empty);
			}
			else{
				String specPOS = tab.getPOS();
				int length = superStrings.length;
				Vector names = new Vector(length);
				Vector pos = new Vector();
				for(int i = 0; i < superStrings.length; i++){
					String currentPOS = superStrings[i][1];
					if (!specPOS.equals("ALL") && !currentPOS.equals(specPOS)) continue;
					Vector row = new Vector(2);
					row.add(superStrings[i][0]);
					row.add(superStrings[i][1]);
					names.add(row);
				}
				copyVec(modelVector, names);
				copyVec(concepts, pos);
			}


			// this line must keep. ( you may need change a little bit.
			// return the first element in the table
			currentWord = (String)((Vector)modelVector.elementAt(0)).elementAt(0);

	 }

}// end of WordNetTable
