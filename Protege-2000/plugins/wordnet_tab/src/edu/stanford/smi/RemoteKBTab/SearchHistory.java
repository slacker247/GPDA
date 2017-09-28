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

import java.util.*;

/** SearchHistory is used to keep track of the redo/undo. */
public class SearchHistory {

    Vector historyRecords; // the search text
    Vector historyNames;   // unique code
    int currentPos;        // indicates current position

    /** Constructor. Current position is used to record the position of current
     *  display result.  Current position is set to -1. */
    public SearchHistory() {
        historyRecords = new Vector();
        historyNames = new Vector();
        currentPos = -1;
    }

    /** Add one record to the history with name and unique name. */
    public void addHisRecord(String name, String uniqname) {
        historyRecords.addElement(name);
        historyNames.addElement(uniqname);
        currentPos = getHisSize() - 1; // reset to the end of the vector
    }

    /** Remove specified record from the history. */
    public void removeHisConcept(int index) {
        historyRecords.remove(index);
        historyNames.remove(index);
        currentPos = getHisSize() - 1;
    }

    /** Reset the whole history. */
    public void clearHis() {
        historyRecords.clear();
        historyNames.clear();
        currentPos = -1;
    }

    /** Return the length of the history. */
    public int getHisSize() {
        return historyRecords.size();
    }

    /** Get the unique name of specified history record. */
    public String getUniqHisName(int index) {
        return (String) (historyNames.elementAt(index));
    }

    /** Get the unique name of last history record and move the pointer back. */
    public String getBackRecord() {
        currentPos --;
        return getUniqHisName(currentPos);
    }

    /** Get the uqique name of the next history record and move the pointer forward. */
    public String getForwardRecord() {
        currentPos ++;
        return getUniqHisName(currentPos);
    }

    /** Get the index of history pointer. */
    public int getHisPosition() {
        return currentPos;
    }
}
