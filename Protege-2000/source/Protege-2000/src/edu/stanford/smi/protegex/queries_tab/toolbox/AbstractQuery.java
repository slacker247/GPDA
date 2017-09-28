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

package edu.stanford.smi.protegex.queries_tab.toolbox;

import java.util.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.util.*;
import edu.stanford.smi.protege.resource.*;
import edu.stanford.smi.protege.ui.*;

/** Abstract Query define the basic requirement for a Query in
    a typical search. */
public abstract class AbstractQuery extends Observable{

   protected Vector qClasses;
   protected Vector qSlots;
   protected Vector qOperations;
   protected Vector qObjects;
   protected Vector qCheckStatus;

   protected Vector qClassesNames;
   protected Vector qSlotsNames;
   protected Vector qObjectsNames;

   private int length;
   private boolean isMatchAll;        // True:  MatchAll
                                      // False: MatchAny

   private String itsName;            // The name for the whole Query

    public AbstractQuery(boolean b) {
        isMatchAll = b;
        qClasses = new Vector();
        qSlots = new Vector();
        qOperations = new Vector();
        qObjects = new Vector();
        qCheckStatus = new Vector();

        qClassesNames = new Vector();
        qSlotsNames = new Vector();
        qObjectsNames = new Vector();
    }

    public void addQuery(Object[] objs, String className, String slotName, String objectName) {
        qClasses.addElement((Cls) objs[0]);
        qSlots.addElement((Slot) objs[1]);
        qOperations.addElement((Object) objs[2]);
        qObjects.addElement(objs[3]);
        qCheckStatus.addElement((String) objs[4]);
        qClassesNames.addElement(className);
        qSlotsNames.addElement(slotName);
        qObjectsNames.addElement(objectName);
    }

    public void addQuery(
        Cls cls,
        Slot slot,
        Object operation,
        Object object,
        String check,
        String className,
        String slotName,
        String objectName) {
        qClasses.addElement(cls);
        qSlots.addElement(slot);
        qOperations.addElement(operation);
        qObjects.addElement(object);
        qCheckStatus.addElement(check);
        qClassesNames.addElement(className);
        qSlotsNames.addElement(slotName);
        qObjectsNames.addElement(objectName);
    }

    public void changed(String status) {
        setChanged();
        notifyObservers(status);
    }

    public void cleanQuery() {
        qClasses.removeAllElements();
        qSlots.removeAllElements();
        qOperations.removeAllElements();
        qObjects.removeAllElements();
        qCheckStatus.removeAllElements();
        qClassesNames.removeAllElements();
        qSlotsNames.removeAllElements();
        qObjectsNames.removeAllElements();
    }

    public String getCheckStatus(int index) {
        return (String) qCheckStatus.elementAt(index);
    }

    public Cls getCls(int index) {
        return (Cls) qClasses.elementAt(index);
    }

    public String getClsName(int index) {
        return (String) qClassesNames.elementAt(index);
    }

    public int getIndex(String name) {
        return qObjectsNames.indexOf(name);
    }

    public String getName() {
        return itsName;
    }

    public Object getObject(int index) {
        return (Object) qObjects.elementAt(index);
    }

    public String getObjectName(int index) {
        return (String) qObjectsNames.elementAt(index);
    }

    public String getOperation(int index) {
        return (String) qOperations.elementAt(index);
    }

    public int getSize() {
        length = qClasses.size();
        return length;
    }

    public Slot getSlot(int index) {
        return (Slot) qSlots.elementAt(index);
    }

    public String getSlotName(int index) {
        return (String) qSlotsNames.elementAt(index);
    }

    public abstract void initialize();

    public boolean isMatchAll() {
        return isMatchAll;
    }

    public void removeQuery(int index) {
        qClasses.removeElementAt(index);
        qSlots.removeElementAt(index);
        qOperations.removeElementAt(index);
        qObjects.removeElementAt(index);
        qCheckStatus.removeElementAt(index);
        qClassesNames.removeElementAt(index);
        qSlotsNames.removeElementAt(index);
        qObjectsNames.removeElementAt(index);
    }

    public void replaceObject(Object obj, int index) {
        qObjects.removeElementAt(index);
        qObjects.insertElementAt(obj, index);
    }

    public void setMatchAll(boolean b) {
        isMatchAll = b;
    }

    public void setName(String name) {
        itsName = name;
    }
}
