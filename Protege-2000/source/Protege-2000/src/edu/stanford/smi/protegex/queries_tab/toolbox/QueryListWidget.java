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

public class QueryListWidget extends AbstractQueryListWidget{
  private Action itsLoadQueryAction;
  private Action itsViewQueryAction;
  private Action itsDeleteQueryAction;

  private InstancesQuery itsQuery;

   /** Define table mouse adapter. Single click will show the search result. Double
     click will start a new presearch. */
   protected class QueryListWidgetMouse extends java.awt.event.MouseAdapter
     {
        QueryListWidget queryList;
        public QueryListWidgetMouse(QueryListWidget t) {
             queryList = t;
        }

        public void mousePressed(java.awt.event.MouseEvent event)
        {
            Object object = event.getSource();
            String item;
            int index;

            if (!(object instanceof JList) ) return;

      index = queryList.getList().getSelectedIndex();
            if (index < 0) return;
            if (itsModel.getSize() == 0) return;

            if(event.getClickCount() == 2)
            {
        InstancesQuery query = itsModel.getQueryAt(index);
        showQuery(query);
            }  else{
            }
    }


} // end of TableMouse


    public QueryListWidget(QueriesTab tab) {
        super(tab);
    }

    public QueryListWidget(String name, QueriesTab tab) {
        super(tab);
        itsName = name;
    }

    public QueryListWidget(String name, QueryListModel model, QueriesTab tab) {
        super(tab);
        itsName = name;
        initialize(model);
    }

    private int checkQueryUniqe(String name) {
        int length = itsModel.getSize();
        if (length < 1)
            return -1;

        for (int i = 0; i < length; i++) {
            String tmpName = ((InstancesQuery) itsModel.getQueryAt(i)).getName();
            if (tmpName.equalsIgnoreCase(name))
                return i;
        }

        return -1;
    }

    private boolean confirmDelete() {
        String text = "Delete the selected items?";
        int result = ModalDialog.showMessageDialog(this.getWidget(), text, ModalDialog.MODE_YES_NO);
        return result == ModalDialog.OPTION_YES;
    }

    private int confirmOverwrite(String name) {
        String text = "The name: [" + name + "] already exists. Would you like to overwrite it?";
        int result = ModalDialog.showMessageDialog(this.getWidget(), text, ModalDialog.MODE_YES_NO_CANCEL);
        return result;
    }

    public void doUpLoad(InstancesQuery query) {
        itsQuery = query;

        itsTab.clearSearch();
        Vector widgets = itsTab.getWidgets();
        JPanel searchWidgets = itsTab.getSearchPanel();

        itsTab.setupRadios(query.isMatchAll());
        itsTab.setQueryName(query.getName());

        SearchWidget widgetPanel0 = (SearchWidget) widgets.elementAt(0);
        widgetPanel0.setSelectedObjects(Helper.createObjs(query, 0), Helper.createNames(query, 0));

        for (int i = 1; i < query.getSize(); i++) {
            int widgetNum = widgets.size();

            SearchWidget widgetPanel = new SearchWidget(itsTab, itsTab.getKnowledgeBase());
            widgetPanel.setSelectedObjects(Helper.createObjs(query, i), Helper.createNames(query, i));
            searchWidgets.add(widgetPanel);
            widgets.addElement(widgetPanel);
        }

        Box emptyBox = itsTab.getEmptyBox();
        searchWidgets.add(emptyBox);

        if (widgets.size() > 1) {
            searchWidgets.revalidate();
            searchWidgets.setPreferredSize(new Dimension(itsTab.getWidgetWidth(), itsTab.getWidgetHeight() * (widgets.size() + 1)));
        }

        searchWidgets.repaint();
        itsTab.setQuery(query);
        itsTab.enableSearch();
        setQueryButtons();

        if (query.getSize() < 2)
            itsTab.setupStatus(false);
        else
            itsTab.setupStatus(true);
    }

    /** This is used to download the current available Query.
           Each SearchWidget corresponds to one item in the InstancesQuery Vector.
    */
    public InstancesQuery downLoadQuery() {
        Vector widgets = itsTab.getWidgets();
        String name;

        name = itsTab.getQueryName();

        InstancesQuery query = new InstancesQuery(itsTab.getMatchAll());

        // create a new Query
        for (int i = 0; i < widgets.size(); i++) {
            SearchWidget tmpSearchWidget = (SearchWidget) widgets.elementAt(i);
            if (tmpSearchWidget.getCls() == null && tmpSearchWidget.getSearchSubject() == null)
                return null;

            Helper.addQuery(tmpSearchWidget, query);
        }

        if (name == null || name.trim().length() < 1) {
            QueryNamePanel panel = new QueryNamePanel();
            int result = ModalDialog.showDialog(this.getWidget(), panel, "Input Query Name", ModalDialog.MODE_OK_CANCEL);
            if (result == ModalDialog.OPTION_OK)
                name = panel.getText();
            itsTab.setQueryName(name);
        }

        if (name != null && name.trim().length() > 0) {
            int index = checkQueryUniqe(name);
            if (index < 0)
                query.setName(name);
            else {

                boolean loop = true;
                itsQuery = itsModel.getQueryAt(index);

                // Compare the two queries, if there are the same, just quite silently
                if (itsTab.compareQuery(itsQuery)) {
                    if (index > -1)
                        itsList.setSelectedIndex(index);
                    return null;
                }

                while (loop) {

                    int returnValue = confirmOverwrite(name);
                    if (returnValue == ModalDialog.OPTION_YES) {
                        query.setName(name);
                        // if there is already a query there, just overwrite it to the model
                        if (itsQuery != null) {
                            loop = false;
                            Helper.copyQuery(query, itsQuery);
                            itsQuery.changed("CHANGED");
                            int position = itsModel.getPosition(itsQuery);
                            if (position > -1)
                                itsList.setSelectedIndex(position);
                            return null;
                        }

                    } else if (returnValue == ModalDialog.OPTION_NO) {
                        QueryNamePanel panel = new QueryNamePanel();
                        int result = ModalDialog.showDialog(this.getWidget(), panel, "Input Query Name", ModalDialog.MODE_OK_CANCEL);
                        if (result == ModalDialog.OPTION_OK) {
                            name = panel.getText();
                            if (name != null && name.trim().length() > 0) {
                                index = checkQueryUniqe(name);
                                if (index < 0) {
                                    query.setName(name);
                                    itsTab.setQueryName(name);
                                    loop = false;
                                }
                            } else {
                                loop = false;
                                return null;
                            }
                        } else {
                            loop = false;
                            return null;
                        }
                    } else { // closed and cancel
                        loop = false;
                        return null;
                    }

                } // end of while
            } // end of else
        } else
            return null;
        return query;

    }

    /** Save the query */
    private Action getAddQueryAction() {
        return new AbstractAction("Save Query", Helper.getIcon("SaveQuery")) {
            public void actionPerformed(ActionEvent event) {

                InstancesQuery query = downLoadQuery();
                if (query == null)
                    return;
                itsModel.addRow(query);

                if (itsModel.getSize() < 1)
                    switchActions(false);
                else
                    switchActions(true);
            }
        };
    }

    //private Action getDeleteQueryAction() {
    private Action getDeleteQueryAction() {
        return new AbstractAction("Delete Query", Icons.getDeleteIcon()) {
            public void actionPerformed(ActionEvent event) {

                int index = itsList.getSelectedIndex();
                if (index < 0)
                    return;

                if (confirmDelete()) {
                    itsModel.deleteRow(index);
                    if (itsModel.getSize() < 1)
                        switchActions(false);
                    else {
                        switchActions(true);
                        if (index < itsModel.getSize())
                            itsList.setSelectedIndex(index);
                        else
                            itsList.setSelectedIndex(itsModel.getSize() - 1);
                    }
                }
                setQueryButtons();

            }
        };
    }

    /** Load the existing query to the search panel */
    private Action getLoadQueryAction() {
        return new AbstractAction("Retrieve Query", Helper.getIcon("LoadQuery")) {
            public void actionPerformed(ActionEvent event) {
                int index = itsList.getSelectedIndex();
                if (index < 0)
                    return;
                InstancesQuery query = itsModel.getQueryAt(index);
                upLoadQuery(query);
            }
        };
    }

    public String getName() {
        return itsName;
    }

    private Action getOpenQueryAction() {
        return new AbstractAction("Open Query File on the Disk", Icons.getOpenProjectIcon()) {
            public void actionPerformed(ActionEvent event) {
                Vector queries = loadFile();
                if (itsModel.getSize() < 1)
                    switchActions(false);
                else
                    switchActions(true);
            }
        };
    }

    private Action getViewQueryAction() {
        return new AbstractAction("View Query", Icons.getViewIcon()) {
            public void actionPerformed(ActionEvent event) {

                int index = itsList.getSelectedIndex();
                if (index < 0)
                    return;
                InstancesQuery query = itsModel.getQueryAt(index);
                showQuery(query);
            }
        };
    }

    public void initialize(QueryListModel model) {
        createComponents("Query Library", model);

        QueryListWidgetMouse queryListMouse = new QueryListWidgetMouse(this);
        itsList.addMouseListener(queryListMouse);

        itsViewQueryAction = getViewQueryAction();
        itsDeleteQueryAction = getDeleteQueryAction();
        itsLoadQueryAction = getLoadQueryAction();

        LabeledComponent c = itsLabeledComponent;
        c.addHeaderButton(itsViewQueryAction);
        c.addHeaderButton(itsLoadQueryAction);
        c.addHeaderButton(itsDeleteQueryAction);

        setQueryButtons();
    }

    private Vector loadFile() { // load file
        JFileChooser chooser = new JFileChooser();
        int status = chooser.showOpenDialog(this);
        Vector queries = null;
        if (status == JFileChooser.APPROVE_OPTION) {
            File file = chooser.getSelectedFile();
            try {

                FileReader fileReader = new FileReader(file);
                BufferedReader bufferedReader = new BufferedReader(fileReader);
                queries = readQuery(bufferedReader);
            } // end of try
            catch (IOException ignored) {
            } // end of catch
        } // end of if

        return queries;

    }

    private boolean preUpLoad(InstancesQuery query) {
        if (!itsTab.isEmptySearchPanel()) {

            if (itsTab.compareQuery(query))
                return false; // the current query are the save as the one which will be loaded.
            if (!itsTab.compareQueries()) {

                String text;
                if (itsTab.getQueryName() != null && itsTab.getQueryName().trim().length() > 1)
                    text = "Do you want to save the changes you made to above [" + itsTab.getQueryName() + "]?";
                else
                    text = "Do you want to save the query you created above? ";
                int result = ModalDialog.showMessageDialog(this.getWidget(), text, ModalDialog.MODE_YES_NO_CANCEL);
                if (result == ModalDialog.OPTION_YES) {
                    // go to save it first and then load it.
                    itsTab.downLoadQuery();
                } else if (result == ModalDialog.OPTION_CANCEL)
                    return false;
            }
        }
        return true;
    }

    private InstancesQuery readInstancesQuery(String str, BufferedReader bufferedReader) {
        String name = Helper.getSpecifiedString("Query Name", str);
        String matchAll = null;
        InstancesQuery query = null;
        boolean match;
        int length;
        String line;

        try {

            if ((line = bufferedReader.readLine()) != null) {
                matchAll = Helper.getSpecifiedString("Match All", line);
            } else
                return null;

            if (matchAll.equalsIgnoreCase("true"))
                match = true;
            else
                match = false;

            query = new InstancesQuery(match);
            query.setName(name);

            if ((line = bufferedReader.readLine()) != null) {
                length = Integer.parseInt(Helper.getSpecifiedString("Length", line));
            } else
                return null;

            for (int i = 0; i < length; i++) {
                if ((line = bufferedReader.readLine()) != null) {
                    StringTokenizer tokenizer = new StringTokenizer(line, "\t");
                    String[] tmpStr = new String[5];
                    int j = 0;
                    while (tokenizer.hasMoreTokens()) {
                        tmpStr[j] = tokenizer.nextToken();
                        j++;
                    }

                    KnowledgeBase kb = itsTab.getKnowledgeBase();

                    Cls cls = kb.getCls(tmpStr[0]);
                    Slot slot = kb.getSlot(tmpStr[1]);

                    String type = Helper.getFirstPart(tmpStr[3]);
                    String valueName = Helper.getSecondPart(tmpStr[3]);
                    Object value;
                    if (type.equalsIgnoreCase("other"))
                        value = valueName;
                    else if (type.equalsIgnoreCase("instance"))
                        value = kb.getInstance(valueName);
                    else if (type.equalsIgnoreCase("cls"))
                        value = kb.getCls(valueName);
                    else if (type.equalsIgnoreCase("query")) {
                        value = itsModel.getQueryWithName(valueName);
                    } else
                        value = null;

                    query.addQuery(cls, slot, tmpStr[2], value, tmpStr[4], tmpStr[0], tmpStr[1], valueName);
                }

            }
        } catch (IOException ignored) {
        }
        return query;
    }

    private Vector readQuery(BufferedReader bufferedReader) {
        String line;
        Vector queries = new Vector();
        try {
            while ((line = bufferedReader.readLine()) != null) {
                if (line.length() < 1)
                    continue;
                if (Helper.getFirstPart(line).equalsIgnoreCase("Query Name")) {
                    InstancesQuery tmpQuery = readInstancesQuery(line, bufferedReader);
                    queries.add(tmpQuery);
                    itsModel.addRow(tmpQuery);
                } else
                    continue;
            }

        } catch (IOException ignored) {
        }

        return queries;
    }

    public void setQueryButtons() {
        if (itsModel.getSize() == 0)
            itsTab.enableQueryButtons(false);
        else
            itsTab.enableQueryButtons(true);
    }

    public void showQuery(InstancesQuery query) {
        itsTab.showDialog(query);
    }

    public void switchActions(boolean b) {
        itsViewQueryAction.setEnabled(b);
        itsDeleteQueryAction.setEnabled(b);
        itsLoadQueryAction.setEnabled(b);

    }

    /** This is used to load the current available query to the search panel. */
    public void upLoadQuery(InstancesQuery query) {
        if (preUpLoad(query))
            doUpLoad(query);
    }
}
