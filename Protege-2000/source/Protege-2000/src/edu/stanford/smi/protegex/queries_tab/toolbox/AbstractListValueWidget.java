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

abstract public class AbstractListValueWidget extends AbstractSlotValueWidget {
  protected static final Comparator theComparator = new FrameComparator();
  protected JList itsList;
  protected JComponent itsComp;

  protected boolean currentValue;
  protected Slot instanceSlot;
  protected Instance itsInstance;
  protected Action itsViewAction, itsSelectAction, itsRemoveAction;
  protected SlotValueWidgetMouse slotValueListMouse;


  protected KnowledgeBaseListener itsKBListener;

    public AbstractListValueWidget(SearchWidget widget) {
        super(widget);
    }

    protected void addActions() {
    }

    protected void addListener() {
        createListener();
    }

    protected void addMouse() {
        slotValueListMouse = new SlotValueWidgetMouse();
        itsList.addMouseListener(slotValueListMouse);
    }

    /** Create components in this relation display. */
    protected void createComponents(String label) {
        itsList = createList();
        JScrollPane scroll = new JScrollPane(itsList);
        scroll.setPreferredSize(new Dimension(WIDTH, HEIGHT));

        addActions();
        addMouse();
        addListener();
    }

    public JList createList() {

        JList list = ComponentFactory.createSingleItemList(null);
        list.setCellRenderer(new FrameRenderer());
        return list;
    }

    protected void createListener() {
    }

    public void removeListener() {
        itsWidget.getKB().removeKnowledgeBaseListener(itsKBListener);
    }

    public void removeMouse() {
        itsList.removeMouseListener(slotValueListMouse);
    }

    public void setDisplayName(String name) {
        ArrayList displayNames = new ArrayList(CollectionUtilities.createCollection(name));
        ComponentUtilities.setListValues(itsList, displayNames);
        itsList.setForeground(Color.red);
        itsList.setToolTipText("The Specified item can not be founded in the Knowledge Base.");
    }

    protected void updateList() {
        ArrayList displayInstances = new ArrayList(CollectionUtilities.createCollection(itsInstance));
        Collections.sort(displayInstances, theComparator);
        ComponentUtilities.setListValues(itsList, displayInstances);
        itsList.setForeground(Color.black);
        itsList.setToolTipText("");
    }
}
