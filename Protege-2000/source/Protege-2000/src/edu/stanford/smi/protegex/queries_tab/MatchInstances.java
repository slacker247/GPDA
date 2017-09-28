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

package edu.stanford.smi.protegex.queries_tab;

import edu.stanford.smi.protege.resource.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.util.*;
import edu.stanford.smi.protege.widget.*;
import edu.stanford.smi.protege.action.*;
import edu.stanford.smi.protege.ui.*;
import java.util.*;

public class MatchInstances {
   private Cls selection;
   private boolean matchOption;
   private Vector widgets;
   private QueriesTab itsTab;

   private Collection instances;
   private Collection resultInstances;          // for combined instances
   private boolean ready;

    public MatchInstances() {
        matchOption = true;
    }

    public MatchInstances(QueriesTab tab, Cls cls, boolean o, Vector widgets) {
        itsTab = tab;
        selection = cls;
        matchOption = o;
        this.widgets = widgets;
    }

    private Collection combineCollection(Vector collectionVec) {

        resultInstances = (Collection) collectionVec.elementAt(0);
        if (resultInstances == null)
            resultInstances = new ArrayList();

        for (int i = 1; i < collectionVec.size(); i++) {
            Collection tmpInstances = (Collection) collectionVec.elementAt(i);
            if (tmpInstances == null)
                continue;
            Iterator j = tmpInstances.iterator();
            while (j.hasNext()) {
                Instance tmpInstance = (Instance) j.next();
                if (resultInstances.contains(tmpInstance))
                    continue;
                else
                    resultInstances.add(tmpInstance);
            }
        }
        return resultInstances;
    }

    public Collection getResult() {
        if (matchOption)
            return instances;
        else
            return resultInstances;
    }

    public Collection search() {

        ready = true;
        for (int i = 0; i < widgets.size(); i++) {
            if (!((SearchWidget) widgets.elementAt(i)).isReady()) {
                ready = false;
                break;
            }
        }

        if (!ready) {
            resultInstances = null;
            instances = null;
            return null;
        }

        // Match all or match any.

        itsTab.getQueryStack().push("Root");
        if (matchOption) {
            instances = ((SearchWidget) widgets.elementAt(0)).search();
            for (int i = 1; i < widgets.size(); i++) {
                instances = ((SearchWidget) widgets.elementAt(i)).search(instances);
                if (itsTab.getQueryStack().isEmpty()) {
                    return null;
                }
            }
            itsTab.getQueryStack().clear();
            return instances;
        } else {
            Vector instancesCol = new Vector();
            for (int i = 0; i < widgets.size(); i++) {
                instancesCol.addElement(((SearchWidget) widgets.elementAt(i)).search());
                if (itsTab.getQueryStack().isEmpty()) {
                    return null;
                }
            }

            itsTab.getQueryStack().clear();
            return combineCollection(instancesCol);
        }
    }
}
