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

public abstract class AbstractQueryListWidget extends JComponent{
   protected JList itsList;
   protected LabeledComponent itsLabeledComponent;

   protected Action itsViewAction, itsSelectAction, itsRemoveAction;
   protected InstancesQuery query;

   protected String itsName;
   protected QueriesTab itsTab;

   protected QueryListModel itsModel;

    public AbstractQueryListWidget(QueriesTab tab) {
        itsTab = tab;
    }

    /** Create components in this relation display. */
    protected void createComponents(String label, QueryListModel model) {

        itsModel = model;
        itsList = createList(itsModel);
        JScrollPane scroll = new JScrollPane(itsList);
        scroll.setPreferredSize(new Dimension(150, 40));

        itsLabeledComponent = new LabeledComponent(label, scroll);
        itsList.setCellRenderer(new QueriesTabRenderer());
    }

    public JList createList() {

        JList list = ComponentFactory.createList(null);
        list.setCellRenderer(new FrameRenderer());
        return list;
    }

    public JList createList(QueryListModel model) {

        JList list = new JList(model);
        list.setCellRenderer(new FrameRenderer());
        return list;
    }

    public JComponent getComponent() {
        return itsLabeledComponent;
    }

    public JList getList() {
        return itsList;
    }

    public QueryListModel getModel() {
        return itsModel;
    }

    public abstract String getName();

    public JPanel getPanel() {
        JPanel itsComp = new JPanel();
        itsComp.setLayout(new BorderLayout(10, 10));
        itsComp.add(itsLabeledComponent, BorderLayout.CENTER);
        return itsComp;
    }

    /** Return the embedded widget in the Query List */
    public JComponent getWidget() {
        return itsLabeledComponent;
    }
}
