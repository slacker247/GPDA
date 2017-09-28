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

/** HistoryData is used to keep a complete record of search result */
public class HistoryData {

    /** title is the string which is shown in the search result */
    private String title;

    /** uniqueName include title and search specification */
    private String uniqueName;
    private String searchWord;
    private Vector tableVector;
    private Vector objectVector;
    private Object searchSpecification;
    private Vector stringArrayVector;

    public HistoryData() {
        tableVector = new Vector();
        objectVector = new Vector();
        stringArrayVector = new Vector();
    }

    /** Generate a new HistoryData from an old one */
    public HistoryData(HistoryData orig) {
        this();
        title = orig.getTitle();
        uniqueName = orig.getUniqueName();
        copyVec(tableVector,orig.getTableVector());
        copyVec(objectVector,orig.getObjectVector());
        copyStringArrayVec(stringArrayVector ,orig.getStringArrayVector());
        searchSpecification = orig.getSearchSpecification();
    }

    /** Return the title of HistoryData. Title is the string which is shown in
     *  the search result */
    public String getTitle() {
        return title;
    }

    /** set the title of HistoryData to newTitle.
     *  Title is the string which is shown in the search result */
    public void setTitle(String newTitle){
        title = newTitle;
    }

    /** set the searchWord */
    public void setSearchWord(String newSearchWord){
        searchWord = newSearchWord;
    }

    /** get the searchWord */
    public String getSearchWord(){
        return searchWord;
    }

    /** get uniqueName. UniqueName include title and search specification */
    public String getUniqueName(){
        return uniqueName;
    }

    /** set uniqueNmae as input string name. UniqueName include title and
     *  search specification */
    public void setUniqueName(String name){
        //uniqueName = name;

        int range = Integer.MAX_VALUE / 3 * 2;
        Random rand = new Random();
        int i = rand.nextInt(range);
        Integer j = new Integer(i);
        uniqueName = name + j.toString();
    }

    /** get objectVector. The element in objectVector is the Object corresponds
     *  to the table row element. */
    public Vector getObjectVector(){
        return objectVector;
    }

    /** set objectVector. The element in objectVector is the Object corresponds
     *  to the table row element. */
    public void setObjectVector(Vector newObjectVector){
        copyVec(objectVector, newObjectVector);
    }

    /** get tableVector */
    public Vector getTableVector(){
        return tableVector;
    }

    /** set tableVector */
    public void setTableVector(Vector newTableVector){
        copyVec(tableVector, newTableVector);
    }

    /** get searchSpecification */
    public Object getSearchSpecification(){
        return searchSpecification;
    }

    /** set searchSpecification */
    public void setSearchSpecification(Object spec){
        searchSpecification = spec;
    }

    /** get stringArrayVector */
    public Vector getStringArrayVector(){
        return stringArrayVector;
    }

    /** set StringArrayVector */
    public void setStringArrayVector(Vector newStringArrayVector){
        copyStringArrayVec(stringArrayVector, newStringArrayVector);
    }

    /** deep copy stringArray vector */
    private void copyStringArrayVec(Vector newStr, Vector oldStr){
        newStr.clear();
        for(int i = 0; i < oldStr.size(); i++) {
            String[] orig = (String[])oldStr.elementAt(i);
            String[] newArray;
            if (orig != null) {
                newArray = new String[orig.length];
                copyHisString(newArray, orig);
            } else {
                newArray = null;
            }
            newStr.addElement(newArray);
        }
    }

    /** deep copy string array */
    private void copyHisString(String[] newStr, String[] oldStr) {
        for (int i=0; i<oldStr.length;i++) {
            String str = oldStr[i];
            if (str != null) {
                str = new String(str);
            }
            newStr[i]= str;
        }
    }

    /** deep copy vector */
    private void copyVec(Vector newvec,Vector oldvec) {
        newvec.clear();
        if (oldvec == null) { return; }
        for (int i=0; i<oldvec.size(); i++) {
            newvec.addElement(oldvec.elementAt(i));
        }
    }
}
