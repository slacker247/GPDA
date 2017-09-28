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

import java.awt.*;
import java.awt.event.*;
import java.util.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.border.*;
import edu.stanford.smi.protege.resource.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.util.*;
import edu.stanford.smi.protege.widget.*;
import edu.stanford.smi.protege.action.*;
import edu.stanford.smi.protegex.queries_tab.toolbox.*;

import edu.stanford.smi.protege.ui.*;
import edu.stanford.smi.protege.ui.Finder.*;
import java.net.*;
import java.io.*;
import java.sql.*;
import javax.swing.table.*;
import javax.swing.tree.*;

public class QueryDialogPanel extends JPanel {
   final static int WIDTH = 690;
   final static int HEIGHT = 70;
   private KnowledgeBase itsKB;
   private QueriesTab itsTab;
   private Vector widgetsQ;
   private JPanel searchQ;
   private ButtonGroup specgroup;
   private JRadioButton matchAll;
   private JRadioButton matchAny;
   private JTextField queryNameField;
   private JPanel queryPanel;
   private boolean match_all;
   private InstancesQuery itsQuery;
   private JButton moreButt, fewerButt, clearSearchButt;
   private Box emptyBox;

    public QueryDialogPanel(InstancesQuery query, QueriesTab tab) {
        itsTab = tab;
        itsKB = itsTab.getKnowledgeBase();
        itsQuery = query;

        queryPanel = this;
        widgetsQ = new Vector();
        searchQ = new JPanel();
        searchQ.setLayout(new BoxLayout(searchQ, BoxLayout.Y_AXIS));

        SearchWidget widgetPanel0 = new SearchWidget(tab, itsKB);

        widgetPanel0.setSelectedObjects(Helper.createObjs(itsQuery, 0), Helper.createNames(itsQuery, 0));
        searchQ.add(widgetPanel0);
        widgetsQ.add(widgetPanel0);
        widgetPanel0.setViewEnabled(false);
        widgetPanel0.removeMouseListener();

        for (int i = 1; i < itsQuery.getSize(); i++) {
            int widgetNum = widgetsQ.size();
            SearchWidget widgetPanel = new SearchWidget(itsTab, itsTab.getKnowledgeBase());
            widgetPanel.setSelectedObjects(Helper.createObjs(itsQuery, i), Helper.createNames(itsQuery, i));
            searchQ.add(widgetPanel);
            widgetsQ.addElement(widgetPanel);
            widgetPanel.setViewEnabled(false);
            widgetPanel.removeMouseListener();
        }
        emptyBox = createEmptyBox();
        searchQ.add(emptyBox);

        if (widgetsQ.size() > 1) {
            searchQ.revalidate();
            searchQ.setPreferredSize(new Dimension(itsTab.getWidgetWidth(), itsTab.getWidgetHeight() * (widgetsQ.size() + 1)));
        }

        queryPanel.setLayout(new BorderLayout(10, 10));
        queryPanel.setBorder(BorderFactory.createTitledBorder(""));

        queryPanel.add(createSearchSpec(), BorderLayout.NORTH);
        queryPanel.add(new JScrollPane(searchQ), BorderLayout.CENTER);
        queryPanel.add(createButtons(), BorderLayout.SOUTH);

        JPanel panel7 = new JPanel();
        JTextField emptytextfield = new JTextField("");
        emptytextfield.setPreferredSize(new Dimension(80, 20));
        emptytextfield.setVisible(false);

        Box box3 = Box.createVerticalBox();
        box3.add(Box.createVerticalStrut(25));
        box3.add(Box.createVerticalGlue());
        box3.add(emptytextfield);

        panel7.add(box3);

        if (widgetsQ.size() < 2)
            setupStatus(false);
        else
            setupStatus(true);
    }

    private boolean checkQueryNames(String queryName) {

        for (int i = 0; i < widgetsQ.size(); i++) {
            if (((SearchWidget) widgetsQ.elementAt(i)).getQueryName() == null)
                continue;
            if (((SearchWidget) widgetsQ.elementAt(i)).getQueryName().equalsIgnoreCase(queryName))
                return false;
        }

        return true;

    }

    // ** Reset the serchWidget settings */
    public void clearSearch() {

        int widgetNum = widgetsQ.size();
        if (widgetNum > 0) {
            for (int i = widgetNum - 1; i > 0; i--) {
                searchQ.remove((JPanel) widgetsQ.elementAt(i));
                widgetsQ.remove(i);
            }
            ((SearchWidget) widgetsQ.elementAt(0)).clearSearch();
            searchQ.revalidate();
            searchQ.setPreferredSize(new Dimension(WIDTH, HEIGHT));
        }
        if (widgetsQ.size() < 2)
            setupStatus(false);
    }

    // Compare between queryDialogPanel and searchTab
    private boolean compareQueries() {
        if (!Helper.compareName(itsTab.getQueryName(), queryNameField.getText().trim()))
            return false;
        if (!Helper.compareMatchStatus(itsTab.getMatchAll(), match_all))
            return false;
        if (!Helper.compareSearchWidgets(itsTab.getWidgets(), widgetsQ))
            return false;
        return true;
    }

    /** Compare the current loading query with another query. */
    public boolean compareQuery(InstancesQuery query) {
        if (!Helper.compareName(query, queryNameField.getText().trim()))
            return false;
        if (!Helper.compareMatchStatus(query, match_all))
            return false;
        if (!Helper.compareSearchWidgets(query, widgetsQ))
            return false;
        return true;
    }

    private JComponent createButtons() {
        JPanel buttPanel = new JPanel();
        moreButt = new JButton("More");
        fewerButt = new JButton("Fewer");

        clearSearchButt = new JButton("Clear");

        buttPanel.setLayout(new GridLayout(1, 3, 3, 3));

        clearSearchButt.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                clearSearch();
            }
        });

        buttPanel.add(moreButt);
        buttPanel.add(fewerButt);
        buttPanel.add(clearSearchButt);

        moreButt.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent event) {
                int widgetNum = widgetsQ.size();
                SlotsModel smodel = (SlotsModel) ((SearchWidget) widgetsQ.elementAt(widgetNum - 1)).getSlotsModel();
                JPanel widgetPanel = new SearchWidget(itsTab, itsKB, smodel.getSlotNames(), smodel.getSlotTypes(), smodel.getSlots());
                ((SearchWidget) widgetPanel).getSearchClass().setCls(((SearchWidget) widgetsQ.elementAt(widgetNum - 1)).getCls());

                searchQ.remove(emptyBox);
                searchQ.add(widgetPanel);
                searchQ.add(emptyBox);

                widgetsQ.addElement(widgetPanel);
                ((SearchWidget) widgetPanel).setViewEnabled(false);

                searchQ.revalidate();
                searchQ.setPreferredSize(new Dimension(WIDTH, HEIGHT * (widgetNum + 1)));
                setupStatus(true);
            }
        });

        fewerButt.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent event) {
                int widgetNum = widgetsQ.size();
                if (widgetNum > 1) {
                    searchQ.remove((JPanel) widgetsQ.elementAt(widgetNum - 1));
                    widgetsQ.remove(widgetNum - 1);

                    searchQ.repaint();
                    searchQ.revalidate();
                    searchQ.setPreferredSize(new Dimension(WIDTH, HEIGHT * (widgetNum - 1)));
                    if (widgetsQ.size() < 2)
                        setupStatus(false);
                }
            }
        });

        return buttPanel;
    }

    private Box createEmptyBox() {
        emptyBox = Box.createVerticalBox();
        emptyBox.add(Box.createVerticalGlue());
        emptyBox.add(Box.createVerticalStrut(200));
        return emptyBox;
    }

    /** Create search specifications. */
    private JComponent createSearchSpec() {
        JPanel specPanel = new JPanel();
        specPanel.setLayout(new BoxLayout(specPanel, BoxLayout.Y_AXIS));
        Box box1 = Box.createHorizontalBox();

        JLabel queryName = new JLabel("Query:");
        queryNameField = new JTextField();
        queryNameField.setText(itsQuery.getName());
        queryNameField.setPreferredSize(new Dimension(60, 20));

        box1.add(queryName);
        box1.add(queryNameField);

        specgroup = new ButtonGroup();

        matchAll = new JRadioButton("Match All", true);
        specgroup.add(matchAll);
        box1.add(matchAll);

        matchAny = new JRadioButton("Match Any", false);
        specgroup.add(matchAny);

        matchAll.setSelected(itsQuery.isMatchAll());
        matchAny.setSelected(!itsQuery.isMatchAll());
        box1.add(matchAny);

        matchAll.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                if (e.getStateChange() == ItemEvent.SELECTED) {
                    match_all = true;
                } else {
                    match_all = false;
                }
            }
        });
        specPanel.add(box1);
        return specPanel;
    }

    private boolean getMatchAll() {
        return match_all;
    }

    // save the content to the itsmodel
    public boolean save() {
        String name = queryNameField.getText().trim();

        if (name == null || name.trim().length() < 1) {
            QueryNamePanel panel = new QueryNamePanel();
            int result = ModalDialog.showDialog(itsTab, panel, "Input Query Name", ModalDialog.MODE_OK_CANCEL);
            if (result == ModalDialog.OPTION_OK)
                name = panel.getText();
        }

        if (name == null || name.length() < 1)
            return false;

        if (!checkQueryNames(name)) {
            ModalDialog.showMessageDialog(itsTab, "This query can not be saved in this name since it include itself.");
            return false;
        }

        match_all = matchAll.isSelected();

        if (compareQuery(itsQuery))
            return false; // There is no changes.

        if (!compareQueries() && !itsTab.compareQueries()) {
            // the queryDialogPanel and searchPanel are different.
            String text = "Do you want to overwrite the query you created in search Tab?";
            int result = ModalDialog.showMessageDialog(itsTab, text, ModalDialog.MODE_YES_NO);
            if (result == ModalDialog.OPTION_NO)
                return false;
        }

        itsQuery.setName(name);
        itsQuery.setMatchAll(matchAll.isSelected());

        // create a new Query
        itsQuery.cleanQuery();
        for (int i = 0; i < widgetsQ.size(); i++) {
            SearchWidget tmpSearchWidget = (SearchWidget) widgetsQ.elementAt(i);
            if (tmpSearchWidget.getCls() == null && tmpSearchWidget.getSearchSubject() == null)
                return false;
            Helper.addQuery(tmpSearchWidget, itsQuery);
        }
        return true;

    }

    /** Switch buttons and radios according to the settings.
          True:  enable the button and radios
          False: disable the button and radios
    */
    public void setupStatus(boolean b) {
        matchAll.setEnabled(b);
        matchAny.setEnabled(b);
        fewerButt.setEnabled(b);
        if (b)
            clearSearchButt.setEnabled(b);

    }

    public void update(Observable instancesQuery, Object arg) {
        clearSearch();
        queryNameField.setText(((InstancesQuery) instancesQuery).getName());
        matchAll.setSelected(((InstancesQuery) instancesQuery).isMatchAll());
        matchAny.setSelected(!((InstancesQuery) instancesQuery).isMatchAll());

        SearchWidget widgetPanel0 = (SearchWidget) widgetsQ.elementAt(0);
        widgetPanel0.setSelectedObjects(
            Helper.createObjs(((InstancesQuery) instancesQuery), 0),
            Helper.createNames(((InstancesQuery) instancesQuery), 0));

        for (int i = 1; i < ((InstancesQuery) instancesQuery).getSize(); i++) {
            int widgetNum = widgetsQ.size();
            SearchWidget widgetPanel = new SearchWidget(itsTab, itsTab.getKnowledgeBase());
            widgetPanel.setSelectedObjects(
                Helper.createObjs(((InstancesQuery) instancesQuery), i),
                Helper.createNames(((InstancesQuery) instancesQuery), i));
            searchQ.add(widgetPanel);
            widgetsQ.addElement(widgetPanel);
        }

        if (widgetsQ.size() > 1) {
            searchQ.revalidate();
            searchQ.setPreferredSize(new Dimension(itsTab.getWidgetWidth(), itsTab.getWidgetHeight() * (widgetsQ.size() + 1)));
        }
    }
}
