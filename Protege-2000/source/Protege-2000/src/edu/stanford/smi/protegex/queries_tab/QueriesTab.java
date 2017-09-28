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

public class QueriesTab extends AbstractTabWidget implements Observer {
   private static final String QUERYNAME = "SearchTab_Query";
   final static int WIDTH = 690;
   final static int HEIGHT = 70;
   private static final String TABNAME = "Queries";
   private static final int MAIN_DEFAULT = 600;
   private static final int HEIGHT_DEFAULT = 400;
   private JTree itsClsTree;
   private KnowledgeBase itsKB;
   private JPanel searchWidgets;
   private JPanel searchPanel;
   private JPanel resultPanel;
   private JPanel searchNorth;
   private JButton moreButt, fewerButt, clearSearchButt;
   private JButton	saveQueryButton;
   private InstancesList itsDirectInstancesList;
   private Vector widgets;
   private boolean match_all;
   private JSplitPane itsMainSplitter;
   private JSplitPane itsSearchSplitter;
   private Cls currentSelectedCls;
     private edu.stanford.smi.protege.model.Frame currentSelectedFrame;
   private JButton searchButt, helpButt;
   private QueryListModel model;
   private QueryListWidget queryList;
   private QueriesTab itsTab;
   private ButtonGroup specgroup;          // button groups for radio buttons
   private JRadioButton matchAll;
   private JRadioButton matchAny;
   private JTextField queryNameField;
   private PropertyList itsPropertyList;
   private int linePosition;
   private InstancesQuery itsQuery;
   private Map itsFrames = new HashMap();					// <Instance or FrameSlotPair, JFrame>
   private Map itsObjects = new HashMap();        // store the InstancesQuery

   Box emptyBox;

   private Stack queryStack;
   private Vector recordQuery;


   private WindowListener itsCloseListener = new WindowAdapter() {
        public void windowClosing(WindowEvent event) {
              JFrame frame = (JFrame) event.getWindow();
            frame.setVisible(false);
            updateHashMap(frame);
            ComponentUtilities.dispose(frame);
            edu.stanford.smi.protege.Application.repaint();
        }
    };

    public QueriesTab() {
        widgets = new Vector();
        itsTab = this;
    }

    private int checkQueryNames() {
        if (queryNameField.getText() == null || queryNameField.getText().length() < 1)
            return 1;
        String queryName = queryNameField.getText().trim();

        for (int i = 0; i < widgets.size(); i++) {
            if (((SearchWidget) widgets.elementAt(i)).getQueryName() == null)
                continue;
            if (((SearchWidget) widgets.elementAt(i)).getQueryName().equalsIgnoreCase(queryName))
                return 2;
        }

        return 0;

    }

    private boolean checkSlotSelections() {
        if (getMatchAll()) {
            for (int i = 0; i < widgets.size(); i++) {
                if (((SearchWidget) widgets.elementAt(i)).getDisplayedSlotName() == null) {
                    return false;
                }
            }
        }
        return true;
    }

    public void clearQueryStack() {
        if (!queryStack.isEmpty())
            queryStack.clear();
    }

    // ** Reset the serchWidget settings */
    public void clearSearch() {
        if (itsQuery != null)
            itsQuery.deleteObserver(this);
        itsQuery = null;
        int widgetNum = widgets.size();
        if (widgetNum > 0) {
            for (int i = widgetNum - 1; i > 0; i--) {
                searchWidgets.remove((JPanel) widgets.elementAt(i));
                widgets.remove(i);
            }
            ((SearchWidget) widgets.elementAt(0)).clearSearch();
            searchWidgets.repaint();
            searchWidgets.revalidate();
            searchWidgets.setPreferredSize(new Dimension(WIDTH, HEIGHT));
        }
        this.setupRadios(true);
        this.setQueryName("");

        // clear the search result
        itsDirectInstancesList.setInstances(null);
        itsDirectInstancesList.getLabeledComponent().setHeaderLabel("Search Results ");

    }

    public boolean compareQueries() {
        String name = getQueryName();
        InstancesQuery query = model.getQueryWithName(name);
        if (query != null) {
            return compareQuery(query);
        }
        return false;

    }

    /** Compare the current loading query with another query. */
    public boolean compareQuery(InstancesQuery query) {
        if (!Helper.compareName(query, queryNameField.getText().trim()))
            return false;
        if (!Helper.compareMatchStatus(query, match_all))
            return false;
        if (!Helper.compareSearchWidgets(query, widgets))
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
                setupStatus(false);
                enableSearch();
            }
        });

        buttPanel.add(moreButt);
        buttPanel.add(fewerButt);
        buttPanel.add(clearSearchButt);

        moreButt.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent event) {
                int widgetNum = widgets.size();
                SlotsModel smodel = (SlotsModel) ((SearchWidget) widgets.elementAt(widgetNum - 1)).getSlotsModel();

                JPanel widgetPanel = new SearchWidget(itsTab, itsKB, smodel.getSlotNames(), smodel.getSlotTypes(), smodel.getSlots());

                ((SearchWidget) widgetPanel).setData(currentSelectedCls);

                ((SearchWidget) widgetPanel).getSearchClass().setCls(((SearchWidget) widgets.elementAt(widgetNum - 1)).getCls());

                searchWidgets.remove(emptyBox);
                searchWidgets.add(widgetPanel);
                searchWidgets.add(emptyBox);
                widgets.addElement(widgetPanel);
                searchWidgets.revalidate();
                searchWidgets.setPreferredSize(new Dimension(WIDTH, HEIGHT * (widgetNum + 1)));

                setupStatus(true);
                enableSearch();
            }
        });

        fewerButt.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent event) {
                int widgetNum = widgets.size();
                if (widgetNum > 1) {
                    searchWidgets.remove((JPanel) widgets.elementAt(widgetNum - 1));
                    widgets.remove(widgetNum - 1);
                    searchWidgets.repaint();
                    searchWidgets.revalidate();
                    searchWidgets.setPreferredSize(new Dimension(WIDTH, HEIGHT * (widgetNum - 1)));
                    if (widgets.size() < 2)
                        setupStatus(false);
                }

                enableSearch();
            }
        });

        return buttPanel;
    }

    private Box createEmptyBox() {
        emptyBox = Box.createVerticalBox();
        emptyBox.add(Box.createVerticalGlue());

        emptyBox.add(Box.createVerticalStrut(250));
        return emptyBox;
    }

    private JComponent createInstancesPanel() {
        itsDirectInstancesList = new InstancesList(getProject());
        itsDirectInstancesList.addSelectionListener(new SelectionListener() {
            public void selectionChanged(SelectionEvent event) {
                Collection selection = itsDirectInstancesList.getSelection();
                Instance selectedInstance;
                if (selection.size() == 1) {
                    selectedInstance = (Instance) CollectionUtilities.getFirstItem(selection);
                } else {
                    selectedInstance = null;
                }
            }
        });
        return itsDirectInstancesList;
    }

    private JComponent createMainSplitter() {
        itsMainSplitter = createLeftRightSplitPane("SearchTab.left_right", MAIN_DEFAULT);
        itsMainSplitter.setLeftComponent(createSearchSplitter());
        itsMainSplitter.setRightComponent(createResultComponents());
        return itsMainSplitter;

    }

    private JComponent createQueryComponents() {
        model = new QueryListModel();
        queryList = new QueryListWidget("Try", model, this);
        return (queryList.getPanel());
    }

    public JPanel createQueryDialogPanel(InstancesQuery query, QueriesTab tab) {
        QueryDialogPanel queryPanel = new QueryDialogPanel(query, tab);
        return queryPanel;
    }

    /** Create right hand side components. */
    private JComponent createResultComponents() {
        return createInstancesPanel();
    }

    private JComponent createSaveQueryComponents() {

        JLabel queryName = new JLabel("Query Name");
        Box box1 = Box.createHorizontalBox();
        box1.add(queryName);
        box1.add(Box.createHorizontalGlue());

        queryNameField = new JTextField();
        queryNameField.setText("");
        queryNameField.setPreferredSize(new Dimension(80, 25));
        Box box2 = Box.createHorizontalBox();
        box2.add(queryNameField);
        box2.add(Box.createHorizontalGlue());

        queryNameField.addCaretListener(new CaretListener() {
            public void caretUpdate(CaretEvent e) {
                enableSearch();
                //enableSave();
            }
        });

        JTextField emptytextfield = new JTextField("");
        emptytextfield.setPreferredSize(new Dimension(20, 25));
        emptytextfield.setVisible(false);

        box2.add(emptytextfield);

        saveQueryButton = new JButton("Add to Query Library");
        saveQueryButton.setToolTipText("Add current query to query library below");
        Icon saveQueryIcon = Helper.getIcon("SaveQuery");
        saveQueryButton.setIcon(saveQueryIcon);
        saveQueryButton.setPreferredSize(new Dimension(170, 25));

        saveQueryButton.addActionListener(new ActionListener() {

            public void actionPerformed(ActionEvent e) {
                if (enableSave())
                    downLoadQuery();
            }
        });

        box2.add(saveQueryButton);

        JPanel search = new JPanel();
        search.setLayout(new BoxLayout(search, BoxLayout.Y_AXIS));
        search.add(box1);
        search.add(box2);
        Box box3 = Box.createVerticalBox();
        box3.add(Box.createVerticalStrut(5));
        search.add(box3);
        return search;

    }

    /** Create a group of control buttons to control search. */
    private JComponent createSearchButton() {
        //searchButt = new JButton("Query");
        searchButt = new JButton("Find");
        searchButt.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                doSearch();
            }
        });
        return searchButt;
    }

    /** Create leftside components */
    private JComponent createSearchComponents() {
        return createSearchParts();
    }

    private JComponent createSearchParts() {
        JPanel searchCom = new JPanel();
        searchCom.setLayout(new BorderLayout(2, 2));

        searchNorth = new JPanel();
        searchNorth.setLayout(new BorderLayout(10, 10));

        searchNorth.add(new JScrollPane(createSearchWidgets()), BorderLayout.CENTER);
        searchNorth.add(createSearchSpec(), BorderLayout.SOUTH);
        searchNorth.setBorder(BorderFactory.createTitledBorder("Query"));
        searchCom.add(searchNorth, BorderLayout.CENTER);
        searchCom.add(createSaveQueryComponents(), BorderLayout.SOUTH);
        return searchCom;
    }

    /** Create search specifications. */
    private JComponent createSearchSpec() {
        JPanel specPanel = new JPanel();
        specPanel.setLayout(new BoxLayout(specPanel, BoxLayout.Y_AXIS));

        Box box1 = Box.createHorizontalBox();

        box1.add(createButtons());
        specgroup = new ButtonGroup();

        matchAll = new JRadioButton("Match All", true);
        specgroup.add(matchAll);
        box1.add(matchAll);

        matchAny = new JRadioButton("Match Any", false);
        specgroup.add(matchAny);
        box1.add(matchAny);

        matchAll.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                if (e.getStateChange() == ItemEvent.SELECTED) {
                    match_all = true;
                    enableSearch();
                } else {
                    match_all = false;
                    enableSearch();
                }
            }
        });

        box1.add(Box.createHorizontalGlue());
        box1.add(createSearchButton());

        specPanel.add(box1);
        Box box3 = Box.createVerticalBox();
        box3.add(Box.createVerticalStrut(5));
        specPanel.add(box3);
        return specPanel;
    }

    /** Create Leftside components. */
    private JComponent createSearchSplitter() {
        itsSearchSplitter = createTopBottomSplitPane("SearchTab.left.top_bottom", HEIGHT_DEFAULT);
        itsSearchSplitter.setTopComponent(createSearchComponents());
        itsSearchSplitter.setBottomComponent(createQueryComponents());
        return itsSearchSplitter;
    }

    private JComponent createSearchWidgets() {
        searchWidgets = new JPanel();
        searchWidgets.setLayout(new BoxLayout(searchWidgets, BoxLayout.Y_AXIS));
        Border bored = BorderFactory.createRaisedBevelBorder();
        SearchWidget widgetPanel = new SearchWidget(this, itsKB);
        searchWidgets.add(widgetPanel);
        widgets.addElement(widgetPanel);
        searchWidgets.add(createEmptyBox());

        return searchWidgets;
    }

    public void dispose() {
        int widgetNum = widgets.size();
        for (int i = 0; i < widgetNum; i++) {
            ((SearchWidget) widgets.elementAt(i)).dispose();
        }
    }

    // ** Do the search to find the instances */
    private void doSearch() {
        MatchInstances match = new MatchInstances(this, currentSelectedCls, match_all, widgets);
        itsDirectInstancesList.setInstances(match.search());
        if (match.getResult() == null) {
            itsDirectInstancesList.getLabeledComponent().setHeaderLabel("Search Results ");
        } else
            itsDirectInstancesList.getLabeledComponent().setHeaderLabel("Search Results " + "(" + match.getResult().size() + ")");
    }

    public void downLoadQuery() {
        InstancesQuery query = queryList.downLoadQuery();
        if (query == null)
            return;
        model.addRow(query);

        int index = model.getPosition(query);
        if (index > -1)
            queryList.getList().setSelectedIndex(index);

        if (model.getSize() < 1)
            queryList.switchActions(false);
        else
            queryList.switchActions(true);

        queryList.setQueryButtons();
    }

    public void enableQueryButtons(boolean b) {
        for (int i = 0; i < widgets.size(); i++) {
            ((SearchWidget) widgets.elementAt(i)).enableQueryButton(b);
        }
    }

    /** Decide whether make the save button enable or not. */
    private boolean enableSave() {
        int status = checkQueryNames();
        switch (status) {
            case 1 :
                return true;
            case 2 :
                ModalDialog.showMessageDialog(this, "This query can not be saved in this name since it requires the result of itself.");
                return false;
            default :
                return true;
        }
    }

    public void enableSearch() {
        if (checkSlotSelections())
            searchButt.setEnabled(true);
        else
            searchButt.setEnabled(false);
    }

    public Box getEmptyBox() {
        return emptyBox;
    }

    /** Organizer the query in a vector of string by line.*/
    public Vector getLine(String result) {

        StringTokenizer tokenizer = new StringTokenizer(result, "\n", true);
        Vector row = new Vector();
        String tmpStr = new String();
        int sign = 0;

        while (tokenizer.hasMoreTokens()) {
            tmpStr = tokenizer.nextToken();
            if (!tmpStr.equals("\n")) {
                row.addElement(tmpStr);
                sign = 0;
            } else {
                if (sign > 0)
                    row.addElement("");
                sign++;
            }
        }
        return row;
    }

    public boolean getMatchAll() {
        return match_all;
    }

    public QueryListModel getModel() {
        return model;
    }

    public String getQueryName() {
        String name = queryNameField.getText();
        return name;
    }

    public Stack getQueryStack() {
        return queryStack;
    }

    public JPanel getSearchPanel() {
        return searchWidgets;
    }

    public int getWidgetHeight() {
        return HEIGHT;
    }

    // Get the vector of searchWidgets
    public Vector getWidgets() {
        return widgets;
    }

    public int getWidgetWidth() {
        return WIDTH;
    }

    public void initialize() {
        // called from the PJ framework to set things up.
        // By this time, we already have a property list

        setLabel(TABNAME);
        queryStack = new Stack();
        setIcon(Icons.getFindIcon());
        itsQuery = null;
        match_all = true;
        itsKB = this.getKnowledgeBase();
        currentSelectedCls = null;
        setTabComponents();

        itsPropertyList = getPropertyList();
        Vector queries = loadQuery();

        if (model.getSize() < 1)
            queryList.switchActions(false);
        else {
            queryList.getList().setSelectedIndex(0);
            queryList.switchActions(true);
        }

        setupStatus(false);
    }

    /** if the search Parts are empty, return true, otherwise, return false. */
    public boolean isEmptySearchPanel() {
        if (!(queryNameField.getText() == null || queryNameField.getText().length() < 1))
            return false;
        if (widgets.size() > 1)
            return false;
        if (((SearchWidget) widgets.elementAt(0)).getSearchClass().getSelectedObject() != null)
            return false;
        if (((SearchWidget) widgets.elementAt(0)).getDisplayedSlotName() != null)
            return false;

        return true;
    }

    private Vector loadQuery() { // load file
        Vector queries = null;
        if (itsPropertyList == null)
            return null;
        queries = readQuery(itsPropertyList.getString(QUERYNAME));

        return queries;

    }

    /** Load the slot values to combobox */
    private void loadSlots(Cls cls) {
        int nSlots = cls.getTemplateSlots().size();
        String slotsStr[] = new String[nSlots];
        String slotsType[] = new String[nSlots];
        Collection slots = cls.getTemplateSlots();

        Iterator i = slots.iterator();
        int j = 0;
        while (i.hasNext()) {
            Slot slot = (Slot) i.next();
            slotsStr[j] = slot.getName();

            if (cls.getTemplateSlotValueType(slot) == ValueType.INSTANCE) {
                slotsType[j] = "INSTANCE";
            } else if (cls.getTemplateSlotValueType(slot) == ValueType.BOOLEAN) {
                slotsType[j] = "BOOLEAN";
            } else if (cls.getTemplateSlotValueType(slot) == ValueType.CLS) {
                slotsType[j] = "CLS";
            } else if (cls.getTemplateSlotValueType(slot) == ValueType.FLOAT) {
                slotsType[j] = "FLOAT";
            } else if (cls.getTemplateSlotValueType(slot) == ValueType.INTEGER) {
                slotsType[j] = "INTEGER";
            } else if (cls.getTemplateSlotValueType(slot) == ValueType.STRING) {
                slotsType[j] = "STRING";
            } else if (cls.getTemplateSlotValueType(slot) == ValueType.SYMBOL) {
                slotsType[j] = "SYMBOL";
            } else {
                slotsType[j] = "INSTANCE";
            }
            j++;
        }

        for (int widgetnum = 0; widgetnum < widgets.size(); widgetnum++) {
            SlotsModel widgetSlotModel = ((SearchWidget) widgets.elementAt(widgetnum)).getSlotsModel();
            String type = null;
            widgetSlotModel.initialize(slotsStr, slotsType, slots);
            ((SearchWidget) widgets.elementAt(widgetnum)).setData(cls);
        }

    }

    public static void main(String[] args) {
        edu.stanford.smi.protege.Application.main(args);
    }

    public InstancesQuery popQuery() {
        return (InstancesQuery) queryStack.pop();
    }

    public void postWarningDialog(String name) {
        ModalDialog.showMessageDialog(this, "Search cannot be executed: Query [" + name + "] requires the result of itself.");
    }

    public void pushQuery(InstancesQuery query) {
        queryStack.push(query);
    }

    private InstancesQuery readInstancesQuery(String str, String content) {
        String name = Helper.getSpecifiedString("Query Name", str);
        String matchAll = null;
        InstancesQuery query = null;
        boolean match;
        int length;
        String line;

        line = (String) ((Vector) getLine(content)).elementAt(linePosition);
        linePosition++;
        if (line != null) {
            matchAll = Helper.getSpecifiedString("Match All", line);
        } else
            return null;

        if (matchAll.equalsIgnoreCase("true"))
            match = true;
        else
            match = false;

        query = new InstancesQuery(match);
        query.setName(name);

        line = (String) ((Vector) getLine(content)).elementAt(linePosition);
        linePosition++;
        if (line != null) {
            length = Integer.parseInt(Helper.getSpecifiedString("Length", line));
        } else
            return null;

        for (int i = 0; i < length; i++) {
            line = (String) ((Vector) getLine(content)).elementAt(linePosition);
            linePosition++;
            if (line != null) {
                StringTokenizer tokenizer = new StringTokenizer(line, "\t");
                String[] tmpStr = new String[5];
                int j = 0;
                while (tokenizer.hasMoreTokens()) {
                    tmpStr[j] = tokenizer.nextToken();
                    j++;
                }
                KnowledgeBase kb = itsTab.getKnowledgeBase();

                Cls cls;
                if (tmpStr[0] == null || tmpStr[0].trim().length() < 1)
                    cls = null;
                else
                    cls = kb.getCls(tmpStr[0]);

                Slot slot;
                if (tmpStr[1] == null || tmpStr[1].trim().length() < 1)
                    slot = null;
                else
                    slot = kb.getSlot(tmpStr[1]);

                String type = Helper.getFirstPart(tmpStr[3]);
                String valueName = Helper.getSecondPart(tmpStr[3]);

                Object value;

                if (type == null || valueName == null)
                    value = null;
                else {
                    if (type.equalsIgnoreCase("other"))
                        value = valueName;
                    else if (type.equalsIgnoreCase("instance")) {
                        String names[] = new String[2];
                        names = Helper.getNames(valueName);
                        value = kb.getInstance(names[0]);
                        tmpStr[3] = names[1];
                    } else if (type.equalsIgnoreCase("cls")) {
                        value = kb.getCls(valueName);
                    } else if (type.equalsIgnoreCase("query")) {
                        String names[] = new String[2];
                        names = Helper.getNames(valueName);
                        value = model.getQueryWithName(names[0]);

                        if (value == null) {
                            QueryRecord record = new QueryRecord(model.getSize(), names[0]);
                            recordQuery.addElement(record);
                        }
                        tmpStr[3] = names[1];
                    } else
                        value = null;
                }

                query.addQuery(cls, slot, tmpStr[2], value, tmpStr[4], tmpStr[0], tmpStr[1], tmpStr[3]);
            }

        }
        return query;
    }

    private Vector readQuery(String content) {

        if (content == null)
            return null;
        String line;
        recordQuery = new Vector();

        Vector queries = new Vector();
        int length = ((Vector) getLine(content)).size();
        if (length > 0) {
            linePosition = 0;
            while (linePosition < length) {
                line = (String) ((Vector) getLine(content)).elementAt(linePosition);
                linePosition++;
                if (line.length() < 1)
                    continue;
                if (Helper.getFirstPart(line).equalsIgnoreCase("Query Name")) {
                    InstancesQuery tmpQuery = readInstancesQuery(line, content);
                    queries.add(tmpQuery);
                    model.addRow(tmpQuery);
                } else
                    continue;
            }

        }

        reLoadQuery();
        return queries;
    }

    private void reLoadQuery() {
        if (recordQuery.size() < 1)
            return;
        for (int i = 0; i < recordQuery.size(); i++) {
            String queryName = ((QueryRecord) recordQuery.elementAt(i)).getName();
            Object value = model.getQueryWithName(queryName);
            if (value != null) {
                int position = ((QueryRecord) recordQuery.elementAt(i)).getPosition();
                InstancesQuery query = model.getQueryAt(position);

                int index = query.getIndex(queryName);
                if (index > -1)
                    query.replaceObject(value, index);

            }
        }
    }

    public void save() {
        itsPropertyList.setString(QUERYNAME, saveQuery());
    }

    private String saveQuery() { // write file
        StringBuffer buf = new StringBuffer();
        String str = null;

        int totrow = model.getSize();

        for (int i = 0; i < totrow; i++) {
            InstancesQuery query = model.getQueryAt(i);
            writeQuery(query, buf);
        }

        return (buf.toString());

    }

    public int searchQueryStack(InstancesQuery query) {
        return queryStack.search(query);
    }

    public void setEditable(boolean b) {
        // do nothing in here
    }

    public void setQuery(InstancesQuery query) {
        if (itsQuery != null)
            itsQuery.deleteObserver(this);
        itsQuery = query;
        if (itsQuery != null) {
            itsQuery.addObserver(this);
        }
    }

    public void setQueryName(String name) {
        queryNameField.setText(name);
    }

    /** Setup the layout of the tab */
    public void setTabComponents() {
        add(createMainSplitter());
    }

    public void setupRadios(boolean b) {
        match_all = b;
        if (b) {
            matchAll.setSelected(b);
        } else {
            matchAny.setSelected(true);
        }
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

    public void showDialog(InstancesQuery query) {
        JPanel panel = createQueryDialogPanel(query, this);
        //int result = ModalDialog.showDialog(this, panel, "Query: " + query.getName(),
        //          ModalDialog.MODE_OK_CANCEL, null, false);
        int result = ModalDialog.showDialog(this, panel, "Query: " + query.getName(), ModalDialog.MODE_OK_CANCEL);
        if (result == ModalDialog.OPTION_OK) {
            if (((QueryDialogPanel) panel).save())
                query.changed("CHANGED");
        }
    }

    public void update(Observable instancesQuery, Object arg) {
        if (instancesQuery == null)
            return;
        if (itsQuery == null)
            return;
        if (queryNameField.getText() == null || queryNameField.getText().length() < 1)
            return;
        if (((String) arg).equalsIgnoreCase("CHANGED")) {

            if (!((InstancesQuery) instancesQuery).getName().equalsIgnoreCase(queryNameField.getText()))
                return;
            if (!compareQuery(((InstancesQuery) instancesQuery)))
                queryList.doUpLoad(((InstancesQuery) instancesQuery));
        }

    }

    public void updateHashMap(JFrame frame) {
        Object o = itsObjects.get(frame);
        itsFrames.remove(o);
        itsObjects.remove(frame);
    }

    //private void writeQuery( InstancesQuery query, BufferedWriter bufferedWriter) {
    private void writeQuery(InstancesQuery query, StringBuffer buf) {
        String temName = query.getName();

        if (temName == null)
            buf.append("Query Name: ");
        else
            buf.append("Query Name: " + temName);
        buf.append("\n");

        String temMatchAll;
        if (query.isMatchAll())
            temMatchAll = "true";
        else
            temMatchAll = "false";
        buf.append("Match All:" + temMatchAll);
        buf.append("\n");

        int length = query.getSize();
        buf.append("Length: " + length);
        buf.append("\n");

        for (int i = 0; i < length; i++) {
            String clsName = " ";
            if (query.getCls(i) != null && query.getCls(i).getName() != null)
                clsName = query.getCls(i).getName();
            buf.append(clsName + "\t");

            String slotName = " ";
            if (query.getSlot(i) != null && query.getSlot(i).getName() != null)
                slotName = query.getSlot(i).getName();
            buf.append(slotName + "\t");

            String operationName = " ";
            if (query.getOperation(i) != null)
                operationName = query.getOperation(i);
            buf.append(operationName + "\t");
            // when we deal with the object value, we need make difference between instance, class, and query
            if (query.getSlot(i) != null && query.getSlot(i).getValueType() == ValueType.INSTANCE) {
                Object obj = query.getObject(i);
                if (obj instanceof InstancesQuery) {
                    String queryName = " ";
                    String queryBrowserName = " ";
                    if ((InstancesQuery) query.getObject(i) != null) {
                        queryName = ((InstancesQuery) query.getObject(i)).getName();
                        queryBrowserName = ((InstancesQuery) query.getObject(i)).getName();

                    }

                    buf.append("Query:" + queryName + "|" + queryBrowserName + "\t");
                } else {
                    String instanceName = " ";
                    String instanceBrowserName = " ";
                    if ((Instance) query.getObject(i) != null) {
                        instanceName = ((Instance) query.getObject(i)).getName();
                        instanceBrowserName = ((Instance) query.getObject(i)).getBrowserText();

                    }
                    buf.append("Instance:" + instanceName + "|" + instanceBrowserName + "\t");
                }
            } else if (query.getSlot(i) != null && query.getSlot(i).getValueType() == ValueType.CLS) {
                buf.append("Cls:" + ((Cls) query.getObject(i)).getName() + "\t");
            } else if (query.getObject(i) != null)
                buf.append("Other:" + query.getObject(i).toString() + "\t");
            else {
            }

            buf.append(query.getCheckStatus(i) + "\t");
            buf.append("\n");
        }

        buf.append("\n");
    }
}
