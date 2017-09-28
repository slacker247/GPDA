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

import javax.swing.*;
import java.io.*;
import java.util.*;
import java.awt.*;
import java.awt.event.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.util.*;
import edu.stanford.smi.protege.resource.*;
import edu.stanford.smi.protege.ui.*;
import edu.stanford.smi.protegex.queries_tab.*;

import edu.stanford.smi.protege.event.*;

public class InstanceSlotValueWidget extends AbstractListValueWidget implements Observer{
  private InstancesQuery itsQuery;
  private Action itsQueryAction;
  private boolean isInstance;

    /** Constructor. */
    public InstanceSlotValueWidget(SearchWidget widget) {
        super(widget);
        constraints = ConstraintsModel.getInstanceConstraints();
        this.label = "instance";
        createComponents(null);
    }

    /** Constructor with label, slot, and actions. */
    public InstanceSlotValueWidget(SearchWidget widget, String label, String slot) {
        super(widget);
        setSlotName(slot);
        this.label = label;
    }

    protected void addActions() {

        LabeledComponent c = new LabeledComponent(" ", itsList);

        itsSelectAction = getSelectInstanceAction();
        itsViewAction = getViewInstanceAction();
        itsRemoveAction = getRemoveInstanceAction();
        itsQueryAction = getSelectQueryAction();

        c.addHeaderButton(itsViewAction);
        c.addHeaderButton(itsSelectAction);
        c.addHeaderButton(itsRemoveAction);
        c.addHeaderButton(itsQueryAction);

        itsComp = c;
    }

    // get the OR set in the collection Vector.
    private Collection combineCollection(Vector collectionVec) {
        Collection resultInstances = (Collection) collectionVec.elementAt(0);
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

    // whether instance is contained in instancesCol
    private boolean containedInstance(Instance instance, Collection instancesCol) {
        if (instancesCol == null)
            return false;
        if (instancesCol.contains(instance))
            return true;
        else
            return false;
    }

    private boolean containInstance(Instance sel, Slot slot, Collection instancesCol) {
        if (sel == null)
            return false;
        Collection instances = sel.getOwnSlotValues(slot);
        Iterator i = instances.iterator();
        while (i.hasNext()) {
            Instance tmpInstance = (Instance) i.next();
            if (containedInstance(tmpInstance, instancesCol))
                return true;
        }
        return false;
    }

    public JList createList() {
        JList list = ComponentFactory.createSingleItemList(null);
        list.setCellRenderer(new QueriesTabRenderer());
        return list;
    }

    /** Create components in this relation display. */
    protected void createListener() {
        itsKBListener = new KnowledgeBaseAdapter() {
            public void instanceDeleted(KnowledgeBaseEvent event) {
                if (itsInstance == null)
                    return;
                edu.stanford.smi.protege.model.Frame frame = event.getFrame();
                if (frame instanceof Instance) {
                    String name = ((Instance) frame).getBrowserText();
                    if (frame.getFrameID().getValue() == itsInstance.getFrameID().getValue()) {
                        removeInstance();
                        setDisplayName(name);
                    }
                }
            }
        };
        itsWidget.getKB().addKnowledgeBaseListener(itsKBListener);
    }

    public void enableQueryButton(boolean b) {
        itsQueryAction.setEnabled(b);
    }

    // This is the key routine to execute the query.
    private Collection executeQuery(InstancesQuery query) {
        boolean matchOption = query.isMatchAll();
        Collection instances;

        if (matchOption) {
            instances = search(query, 0);
            for (int i = 1; i < query.getSize(); i++) {
                instances = search(query, i, instances);
            }
            return instances;
        } else {
            // Match Any
            Vector instancesCol = new Vector();
            for (int i = 0; i < query.getSize(); i++) {
                instancesCol.addElement(search(query, i));
            }
            return combineCollection(instancesCol);
        }
    }

    /** Return the relation display component. */
    public JComponent getComponent() {
        return itsComp;
    }

    /** Get the data from the text area and return it as a string array. */
    public Object[] getData() {
        Object[] itsData = new Object[1];

        if (isInstance) {
            if (itsInstance == null)
                return null;
            itsData[0] = itsInstance;
        } else {
            if (itsQuery == null)
                return null;
            itsData[0] = itsQuery;
        }
        return itsData;

    }

    private Action getRemoveInstanceAction() {
        return new AbstractAction("Remove Instance", Icons.getRemoveIcon()) {
            public void actionPerformed(ActionEvent event) {
                removeInstance();
            }
        };
    }

    /** Get the selected string in text area and return them as a string array. */
    public String[] getSelectedItems() {
        String[] text = new String[1];
        if (isInstance)
            text[0] = itsInstance.getBrowserText();
        else
            text[0] = itsQuery.getName();
        return text;
    }

    /** Get the selected object which is an instance for instance slot. */
    public Object getSelectedObject() {
        if (isInstance)
            return (Instance) itsInstance;
        else
            return (InstancesQuery) itsQuery;
    }

    /** Get Select(+) Instance Action. */
    private Action getSelectInstanceAction() {
        return new AbstractAction("Select Instance", Icons.getAddIcon()) {
            public void actionPerformed(ActionEvent event) {
                if (instanceSlot == null)
                    return;
                Collection clses;
                Instance instance;
                clses = instanceSlot.getAllowedClses();
                instance = DisplayUtilities.pickInstance(itsComp, clses);

                if (instance != null) {
                    isInstance = true;
                    setDisplayedInstance(instance);
                    itsInstance = instance;
                    itsQuery = null;
                }
                setActionsEnabled(true);
            }
        };
    }

    /** Get Select(+) Instance Action. */
    private Action getSelectQueryAction() {
        return new AbstractAction("Select Query", Icons.getQueryIcon()) {
            public void actionPerformed(ActionEvent event) {

                Collection queries;
                queries = getTab().getModel().getQueries();
                InstancesQuery query = null;
                Collection selectQueries;

                selectQueries = pickQueries(itsComp, queries, "Pick Query");
                if (selectQueries != null) {
                    query = (InstancesQuery) CollectionUtilities.getFirstItem(selectQueries);
                }

                if (query != null) {
                    isInstance = false;
                    itsQuery = query;
                    setDisplayedQuery(itsQuery);
                    itsInstance = null;
                }

                setActionsEnabled(true);
            }
        };
    }

    /** Get slot return the current slot. */
    public Slot getSlot() {

        if (slotName == null)
            return null;
        Slot slot = itsWidget.getKB().getSlot(slotName);
        return slot;
    }

    private int getTestIndex(String constraint) {
        for (int i = 0; i < constraints.length; i++)
            if (constraint.toLowerCase().equals(constraints[i]))
                return i;
        return -1;
    }

    private Collection getValue() {
        Collection value;
        if (specification.getValue() == null)
            return null;
        Object obj = specification.getValue();

        if (obj instanceof InstancesQuery) {
            if (itsTab.searchQueryStack((InstancesQuery) obj) != -1) {
                itsTab.postWarningDialog(((InstancesQuery) obj).getName());
                itsTab.clearQueryStack();
                return null;
            }

            if (itsTab.getQueryStack().isEmpty())
                return null;
            itsTab.pushQuery((InstancesQuery) obj);
            value = executeQuery((InstancesQuery) obj);
            if (itsTab.getQueryStack().isEmpty())
                return null;
            itsTab.popQuery();
        } else {
            value = CollectionUtilities.createCollection(obj);
        }
        return value;
    }

    /** View the instance. */
    private Action getViewInstanceAction() {
        return new AbstractAction("View Instance(Query)", Icons.getViewIcon()) {
            public void actionPerformed(ActionEvent event) {
                viewObject();
            }
        };
    }

    /** Return the embedded widget in the relation display, the JTextArea. */
    public JComponent getWidget() {
        return itsComp;
    }

    /** Set the slot not a single value slot. */
    public boolean isSlotSingleValued() {
        return true;
    }

    private Collection mergeCollection(Collection instances1, Collection instances2) {
        Collection resultInstances = new ArrayList();

        Iterator j = instances1.iterator();
        while (j.hasNext()) {
            Instance tmpInstance = (Instance) j.next();
            if (instances2.contains(tmpInstance))
                resultInstances.add(tmpInstance);
        }

        return resultInstances;
    }

    private Collection pickQueries(Component component, Collection allowedQueries, String label) {
        Collection queries = Collections.EMPTY_LIST;
        if (allowedQueries == null)
            return queries;
        if (!allowedQueries.isEmpty()) {
            SelectPanel panel = new SelectPanel(allowedQueries);

            int result = ModalDialog.showDialog(component, panel, label, ModalDialog.MODE_OK_CANCEL);

            if (result == ModalDialog.OPTION_OK) {
                queries = panel.getSelection();
            }
        }
        return queries;
    }

    private void removeDisplayedInstance() {
        replaceInstance(null);
        updateList();
    }

    private void removeInstance() {
        removeDisplayedInstance();
        replaceQuery(null);
        setActionsEnabled(false);
    }

    private void replaceInstance(Instance instance) {
        itsInstance = instance;
    }

    private void replaceQuery(InstancesQuery q) {
        if (itsQuery != null)
            itsQuery.deleteObserver(this);
        itsQuery = q;
        if (itsQuery != null)
            itsQuery.addObserver(this);
    }

    public Collection search() {
        if (specification.getType() == null)
            return null;
        String slotType = specification.getType();
        if (!slotType.toLowerCase().equals("instance"))
            return null;

        // Here we first test the function based on the simplest Instance cases
        ArrayList resultInstances = new ArrayList();
        Collection instances = itsInstances;
        Iterator i = instances.iterator();
        Slot slot = itsWidget.getKB().getSlot(specification.getName());
        Collection value = getValue();
        while (i.hasNext()) {
            Instance instance = (Instance) i.next();
            if (testInstance(getTestIndex(specification.getConstraint()), instance, slot, value))
                resultInstances.add(instance);
        }
        return resultInstances;
    }

    // No Restriction
    private Collection search(InstancesQuery query, int index) {

        Cls cls = query.getCls(index);
        Slot slot = query.getSlot(index);
        String operation = query.getOperation(index);
        Object obj = query.getObject(index);

        String status = (String) query.getCheckStatus(index);

        int style = Integer.parseInt((String) query.getCheckStatus(index));

        Collection selectInstances = itsWidget.getInstances(cls, style, slot);
        String slotType = Helper.getSlotType(slot);
        SlotValueWidget widget = itsWidget.getMatchSlotWidget(slotType);

        // Now call the suitable search() method inside the slotvalueWidget.
        return widget.search(selectInstances, slot, operation, obj);
    }

    // instances is used to restrict the scope of search
    private Collection search(InstancesQuery query, int index, Collection instances) {

        if (instances == null || instances.size() < 1)
            return null;

        Cls cls = query.getCls(index);
        Slot slot = query.getSlot(index);
        String operation = query.getOperation(index);
        Object obj = query.getObject(index);

        String status = (String) query.getCheckStatus(index);
        int style = Integer.parseInt((String) query.getCheckStatus(index));

        Collection selectInstances = itsWidget.getInstances(cls, style, slot);
        String slotType = Helper.getSlotType(slot);
        if (slotType == null)
            return null;
        SlotValueWidget widget = itsWidget.getMatchSlotWidget(slotType);
        if (widget == null)
            return null;
        return widget.search(mergeCollection(selectInstances, instances), slot, operation, obj);

    }

    // This is used for Query test
    public Collection search(Collection instances, Slot slot, String operation, Object obj) {
        ArrayList resultInstances = new ArrayList();
        Iterator i = instances.iterator();
        Collection value;
        if (obj instanceof InstancesQuery) {
            if (itsTab.searchQueryStack((InstancesQuery) obj) != -1) {
                itsTab.postWarningDialog(((InstancesQuery) obj).getName());
                itsTab.clearQueryStack();
                return null;

            }

            if (itsTab.getQueryStack().isEmpty())
                return null;
            itsTab.pushQuery((InstancesQuery) obj);

            value = executeQuery((InstancesQuery) obj);
            if (itsTab.getQueryStack().isEmpty())
                return null;

            itsTab.popQuery();
        } else {
            value = CollectionUtilities.createCollection(obj);
        }

        while (i.hasNext()) {
            Instance instance = (Instance) i.next();
            if (testInstance(getTestIndex(operation), instance, slot, value))
                resultInstances.add(instance);
        }
        return resultInstances;
    }

    public void setActionsEnabled(boolean b) {

        if (isViewEnabled)
            itsViewAction.setEnabled(b);
        else
            itsViewAction.setEnabled(false);
        itsRemoveAction.setEnabled(b);

    }

    /** Set the specified string to the text area. */
    public void setData(Object[] data) {
        removeDisplayedInstance();
        setActionsEnabled(false);
        itsWidget.setReady(true);
        setActionsEnabled(false);

        itsInstance = null;
        itsQuery = null;
        if (data == null || (String) data[0] == null) {
            return;
        }

        slotName = (String) data[0];
        instanceSlot = getSlot();
        if (instanceSlot == null)
            return;
    }

    private void setDisplayedInstance(Instance instance) {
        isInstance = true;
        replaceInstance(instance);
        replaceQuery(null);
        updateList();
    }

    private void setDisplayedQuery(InstancesQuery q) {
        isInstance = false;
        replaceQuery(q);
        replaceInstance(null);
        updateListQ();
    }

    public void setSelectedObject(Object obj) {
        if (obj instanceof InstancesQuery) {
            setDisplayedQuery((InstancesQuery) obj);
        } else {
            setDisplayedInstance((Instance) obj);
        }
    }

    public void setViewEnabled(boolean b) {
        isViewEnabled = b;
        itsViewAction.setEnabled(isViewEnabled);
    }

    public void showInstance(Instance instance) {
        if (instance != null)
            itsWidget.getKB().getProject().show(instance);
    }

    public void showQuery(InstancesQuery query) {
        itsTab.showDialog(query);
    }

    /** Main test subroutine for Instance slot. */
    private boolean testInstance(int testIndex, Instance instance, Slot slot, Collection instances) {
        boolean testResult = false;
        if (instance == null)
            return testResult;

        if (slot == null) {
            return false;
        }

        Instance tmpInstance = (Instance) instance.getOwnSlotValue(slot);
        if (tmpInstance == null && testIndex == 0)
            return false;
        if (tmpInstance == null && testIndex == 1)
            return true;

        String instanceVal = tmpInstance.getBrowserText();

        switch (testIndex) {
            case 0 : // contains
                return containInstance(instance, slot, instances);
            case 1 : // does not contain
                return (!containInstance(instance, slot, instances));
            default :
                break;
        }
        return testResult;
    }

    public void update(Observable instancesQuery, Object arg) {
        if (itsQuery == null)
            return;
        if (((String) arg).equalsIgnoreCase("DELETED")) {
            String name = itsQuery.getName();
            removeInstance();
            setDisplayName(name);
        }
    }

    // update the Query list
    private void updateListQ() {
        ArrayList displayQueries = new ArrayList(CollectionUtilities.createCollection(itsQuery));
        ComponentUtilities.setListValues(itsList, displayQueries);
        itsList.setForeground(Color.black);
        itsList.setToolTipText("");
    }

    public void viewObject() {
        if (instanceSlot != null && itsInstance != null)
            showInstance(itsInstance);
        else if (itsQuery != null && instanceSlot != null)
            showQuery(itsQuery);
        else
            return;

    }
}
