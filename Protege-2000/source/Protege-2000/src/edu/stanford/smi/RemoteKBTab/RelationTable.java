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

/** Interface that all RelationTable's must implement.
 */
package edu.stanford.smi.RemoteKBTab;

import java.util.*;
import javax.swing.*;
import java.io.*;

public interface RelationTable {

  /** Get the column names. */
  public String[] getTableLabels();

  /** Get the searchResult content. */
	public String getSearchResultText();

	/** Get the table Object vector. */
	public Vector getTableVecObj();

  /** Set the table Object vector. */
	public void setTableVecObj(Vector objVec);

	/** Get the table String vector. */
	public Vector getModelVector();

  /** Set table model vector. */
	public void setModelVector(Vector strVec);

  /** Performs the search for the specified string with the given search context.
      SetData will be called shortly later with the same array of strings to be
      displayed, so it is not necessary to update the display yet.
	 */
  public void search(String targetStr);

  /** Return the component which will be shown. Usually it is a scrollpane. */
  public JComponent getComponent();

  /** Return the table. */
  public JTable getTable();

  /** Return the Relate Table Model. */
  public RelateTableModel getTableModel();

  /**  matchnames is a Vector of Vectors which will be used to fill
	     the tableModel.  These are all of the Strings that will appear
	     in the table.  matchObjects are optional corresponding objects for each
	     row, so matchObjects should have a size less than or equal to the
	     size of matchnames.
	 */
  public void setData(Vector matchnames, Vector matchObjects);

}