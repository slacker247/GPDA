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
import javax.swing.event.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.util.*;
import edu.stanford.smi.protege.resource.*;
import edu.stanford.smi.protege.ui.*;
import edu.stanford.smi.protegex.queries_tab.*;
import edu.stanford.smi.protege.event.*;

public class ClassSelectWidget extends AbstractListValueWidget{

    /** Constructor. */
    public ClassSelectWidget(SearchWidget widget) {
        super(widget);
        constraints = ConstraintsModel.getClsConstraints();
        this.label = "cls";
        createComponents(null);
        setActionsEnabled(false);
    }

    /** Constructor with label, slot, and actions. */
    public ClassSelectWidget(SearchWidget widget, String label, String slot) {
        super(widget);
        setSlotName(slot);
        this.label = label;
    }

    protected void addActions() {

        LabeledComponent c = new LabeledComponent("Class", itsList);
        itsSelectAction = getSelectClsAction();
        itsViewAction = getViewClsAction();
        itsRemoveAction = getRemoveClsAction();

        c.addHeaderButton(itsViewAction);
        c.addHeaderButton(itsSelectAction);
        c.addHeaderButton(itsRemoveAction);
        itsComp = c;
    }

    // This is used to clear the display only.
    public void clearList() {
        ArrayList displayInstances = new ArrayList(CollectionUtilities.createCollection(null));
        Collections.sort(displayInstances, theComparator);
        ComponentUtilities.setListValues(itsList, displayInstances);
    }

    /** Create components in this relation display. */
    protected void createListener() {
        itsKBListener = new KnowledgeBaseAdapter() {
            public void clsDeleted(KnowledgeBaseEvent event) {
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

            public void slotDeleted(KnowledgeBaseEvent event) {
                edu.stanford.smi.protege.model.Frame frame = event.getFrame();
                if (frame instanceof Slot) {
                    String name = ((Slot) frame).getName();
                }
            }
        };
        itsWidget.getKB().addKnowledgeBaseListener(itsKBListener);
    }

    public Cls getCls() {
        return (Cls) itsInstance;
    }

    /** Return the relation display component. */
    public JComponent getComponent() {
        return itsComp;
    }

    /** Get the data from the text area and return it as a string array. */
    public Object[] getData() {
        String[] itsData = new String[1];
        if (itsInstance == null)
            return null;
        itsData[0] = itsInstance.getBrowserText();

        return itsData;
    }

    private Action getRemoveClsAction() {
        return new AbstractAction("Remove Cls", Icons.getRemoveIcon()) {
            public void actionPerformed(ActionEvent event) {
                removeInstance();
            }
        };
    }

    /** Get Select(+) Instance Action. */
    private Action getSelectClsAction() {
        return new AbstractAction("Select Cls", Icons.getAddIcon()) {
            public void actionPerformed(ActionEvent event) {
                Instance instance;
                instanceSlot = getSlot();
                if (instanceSlot != null) {
                    Collection clses;
                    clses = instanceSlot.getTemplateSlotClses();
                    itsWidget.setSelectSlotName();

                    if (clses.size() > 0) {
                        instance = DisplayUtilities.pickCls(itsComp, clses);
                    } else
                        instance = null;
                } else {
                    Collection clses = DisplayUtilities.pickClses(itsComp, project.getKnowledgeBase());
                    itsWidget.setSelectSlotName();
                    if (clses == null) {
                        instance = null;
                    } else {
                        instance = null;
                        Iterator j = clses.iterator();
                        while (j.hasNext()) {
                            instance = (Instance) j.next();
                            break;
                        }
                    }
                }

                if (instance != null) {
                    setDisplayedInstance(instance);
                    setActionsEnabled(true);
                }
            }
        };
    }

    /** Get the selected string in text area and return them as a string array. */
    public String[] getSelectedItems() {
        String[] text = new String[1];
        text[0] = itsInstance.getBrowserText();
        return text;
    }

    /** Get the selected object which is a class for class slot. */
    public Object getSelectedObject() {
        return getCls();
    }

    /** Get slot return the current slot. */
    private Slot getSlot() {
        slotName = itsWidget.getSearchSubject();
        if (slotName == null)
            return null;
        Slot slot = itsWidget.getKB().getSlot(slotName);
        return slot;
    }

    /** View the instance. */
    private Action getViewClsAction() {
        return new AbstractAction("View Cls", Icons.getViewIcon()) {
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

    private void removeDisplayedInstance() {
        replaceInstance(null);
        updateList();
        valueChanged();
    }

    private void removeInstance() {
        itsWidget.setSelectSlotName();
        removeDisplayedInstance();
        setActionsEnabled(false);
    }

    private void replaceInstance(Instance instance) {
        itsInstance = instance;
    }

    public Collection search(Collection instances, Slot slot, String operation, Object obj) {
        return null;
    }

    public void setActionsEnabled(boolean b) {
        if (isViewEnabled)
            itsViewAction.setEnabled(b);
        else
            itsViewAction.setEnabled(false);
        itsRemoveAction.setEnabled(b);

    }

    /** Overwrite the setCls to setup the selected cls */
    public void setCls(Cls cls) {
        selection = cls;
        itsInstance = (Instance) selection;
        // Note: to remove this line might be wrong
        setDisplayedInstance(itsInstance);

    }

    /** Set the specified string to the text area. */
    public void setData(Object[] data) {
        itsList.clearSelection();
        itsWidget.setReady(true);

        setActionsEnabled(false);

        itsInstance = null;
        if (data == null || (String) data[0] == null) {
            return;
        }

        slotName = (String) data[0];
        instanceSlot = getSlot();
        if (instanceSlot == null)
            return;

    }

    /** Set the input instance as the selected classs */
    private void setDisplayedInstance(Instance instance) {
        replaceInstance(instance);
        updateList();
        valueChanged();
    }

    public void setSelectedObject(Object obj) {
        setDisplayedInstance((Instance) obj);
    }

    public void setViewEnabled(boolean b) {
        isViewEnabled = b;
        itsViewAction.setEnabled(isViewEnabled);
    }

    public void showInstance(Instance instance) {
        if (instance != null)
            itsWidget.getKB().getProject().show(instance);
    }

    private void valueChanged() {
        String className;
        if (itsInstance == null) {
            className = null;
        } else
            className = itsInstance.getBrowserText();

        if (className == null || className.length() < 1) {
            itsInstance = null;
            setActionsEnabled(false);
            itsWidget.setClass2(itsInstance);
            return;
        }

        if (className != null && itsInstance != null) {
            setActionsEnabled(true);
            itsWidget.setClass2(itsInstance);
        }

    }

    public void viewObject() {
        if (itsInstance == null)
            return;
        showInstance(itsInstance);
    }
}
