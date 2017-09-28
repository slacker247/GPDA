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

/**RemoteKBTab is the main tab that will appear on screen.
*  This tab has three main subpanels.  The left most has the search and do and
*  undo buttons.  When pressing do and undo, the search textfield will
*  be the one updated, so there is only one search word on screen at any time.
*  The center panel has all of the RelationDisplays that the implementor has
*  created.  Each panel shows one relation on the search term.  The rightmost
*  panel shows the instance and class hierarchy of the current Protege
*  knowledge base that will be appended by this tab.
*/

// java
import java.awt.*;
import java.io.*;
import java.awt.dnd.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.tree.*;
import java.util.*;

// stanford - protege
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.resource.*;
import edu.stanford.smi.protege.ui.*;
import edu.stanford.smi.protege.util.*;
import edu.stanford.smi.protege.widget.AbstractTabWidget;

// stanford - remotekb
import edu.stanford.smi.RemoteKBTab.toolbox.*;

/** Abstract main class. The user tab need extend this class. */
public abstract class RemoteKBTab extends AbstractTabWidget {
    private static final int MAINLEFT_DEFAULT = 300;
    private static final int RESULTLEFT_DEFAULT = 400;
    private static final int INSTANCELEFT_DEFAULT = 120;

    private KnowledgeBase kb;
    //private JButton addallinstancebutt;
    private JButton classButton, instanceButton;
    protected JButton searchButton;
    protected JTextField searchField;
    private DirectInstancesList itsDirectInstancesList;
    private InstanceDisplay itsInstanceDisplay;
    private JTree itsClsTree;

    private Instance currentSelectedInstance;
    private Cls currentSelectedCls, thisClass;
    private edu.stanford.smi.protege.model.Frame currentSelectedFrame;
    private JProgressBar progress;
    private int progressCount;

    protected RelationDisplay[] displays;
    protected RelationTable relationTable;
    protected CommonMouse comMouse;

    //protected Hashtable historyHash, displayHash;
    protected Hashtable displayHash;
    protected HashMap historyHash;

    protected SearchHistory searchHistory;
    protected HistoryData displayData;
    protected Vector displayRecord;
    protected JLabel hisIndicator, searchResult;
    protected JButton backbutt, forwardbutt;

    protected TableMouse tablemouse;
    protected Vector classActions;
    protected Vector instanceActions;

    protected String searchTerm;
    protected Object searchObj;
    protected RemoteKBTabClsTreeFinder clsFinder;
    protected RelationPanel relationPanel;

    private static final String INSTANCES_WIDGET = "artab_instances_widget";
    private static final String PROPERTY_CLSES_WIDGET = "artab_widget";

    public RemoteKBTab() {
    }

    /** Classes that inherit from AbstractWidget should implement initialize()
     *  which is the signal from Protege that the project has been set and that
     *  this tab should draw itself.
    */
    public void initialize(){
        classActions = new Vector();
        instanceActions = new Vector();

        setup();

	kb = getProject().getKnowledgeBase();

	setLayout(new BorderLayout(6,6));
	add(createDisplaySplitter(), BorderLayout.CENTER);  //AbstractWidget inherits from a JComponent
        //historyHash = new Hashtable();
        historyHash = new HashMap();
        displayHash = new Hashtable();
        searchHistory = new SearchHistory();
        updateLabel();
        displayRecord = new Vector();
    }


    /** creates a split pane where the left component has the search panel */
    private JComponent createDisplaySplitter() {
        JSplitPane displayPane = createLeftRightSplitPane("RemoteKBTab.left_right", MAINLEFT_DEFAULT);
	displayPane.setLeftComponent(createSearchComponent());
	displayPane.setRightComponent(createResultSplitter());
	return displayPane;
    }

    public Vector getClassActions() {
        return classActions;
    }

    public Vector getInstanceActions() {
        return instanceActions;
    }

    /** Return the history hashtable.
     *  There are two hashtables in the system. History hash table is used to
     *  record the search text and specification. Display hash table is used to
     *  record search result. */
    public HashMap getHisHash() {
        return this.historyHash;
    }

    /** Return the display hash table. There are two hashtables in the system.
     *  History hash table is used to record the search text and specification.
     *  Display hash table is used to record search result. */
    public Hashtable getDisplayHash() {
        return this.displayHash;
    }

    /** Set whether the table  is a component or not. */
    private boolean hasTable() {
        return (relationTable != null);
    }

    /** Progress bar increases by one unit. */
    public void searchProgress() {
        progressCount++;
        progress.setValue(progressCount);
        progress.paintImmediately(0,0,progress.getSize().width, progress.getSize().height);
    }

    /** Progress bar increases by specifed units. */
    public void searchProgress(int step) {
        progressCount= progressCount + step;
        progress.setValue(progressCount);
        progress.paintImmediately(0,0,progress.getSize().width, progress.getSize().height);
    }

    /** Setup the initial value of progress bar. */
    public void initProgress(int max) {
        progress.setMaximum(max);
        progress.setValue(0);
        progressCount = 0;
    }

    /** Reset the progress bar. */
    public void endProgress() {
        progress.setMaximum(0);
        progress.setValue(0);
        progressCount = 0;
    }

    /** This is used to enable the search text field and search button */
    public void setBusy(boolean busy){
        searchField.setEditable(!busy);
        for ( int i = 0; i< displays.length; i++ ) {
            relationPanel.getTabbedPane().setEnabledAt(i, !busy);
        }

        if (busy) {
            setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        }

        if (!busy) {
            endProgress();
            setCursor(Cursor.getDefaultCursor());
        }
    }

    /** Undo the search. Display last search result, search text and
     *  search specification. */
    private void unDo() {
        if (searchHistory.getHisPosition() > 0) {
            String obj = searchHistory.getBackRecord();
            restoreHistory((HistoryData)historyHash.get(obj));
            hisIndicator.setText("" + (searchHistory.getHisPosition()+1) );
        } else {
            // hisConcept position is 0
            // do nothing
            hisIndicator.setText("" + (searchHistory.getHisPosition()+1) );
        }
        updateLabel();
    }

    /** Redo the search. Display next search result, search text and search
    *  Specification. */
    private void reDo() {
        if (searchHistory.getHisPosition() + 1 < searchHistory.getHisSize()) {
            String obj = searchHistory.getForwardRecord();
	    restoreHistory((HistoryData)historyHash.get(obj));
            hisIndicator.setText("" + (searchHistory.getHisPosition()+1) );
	} else {
            // hisConcept position is 0
	    hisIndicator.setText("" + (searchHistory.getHisPosition()+1) );
	}
	updateLabel();
    }


    protected void restoreSpecHistory(Object item) {
    }

    /** The user need handle the order of restoreHistory. They can set them up.
    *  However, The order should be consistent.
    */
    protected void restoreHistory(HistoryData hisData) {
        String searchWord = hisData.getSearchWord();
        searchField.setText(searchWord);
        restoreSpecHistory(hisData.getSearchSpecification());
        String unique = hisData.getUniqueName();

        if (!hasTable()) {
            if (displayHash.containsKey(unique)) {
                restoreDisplay((HistoryData)displayHash.get(unique));
            }
        } else {
            Vector tableVec = new Vector();
	    copyVec(tableVec, hisData.getTableVector());
	    relationTable.setModelVector(tableVec);

	    Vector tableVecObj = new Vector();
	    copyVec(tableVecObj, hisData.getObjectVector());
	    relationTable.setTableVecObj(tableVecObj);
	    relationTable.setData(tableVec, tableVecObj);

            String uniqueName = new String();
            if (tableVec != null && tableVec.size() > 0) {
            uniqueName = getUniqueName(((Vector)tableVec.elementAt(0)).elementAt(0).toString());
            } else {
                uniqueName = hisData.getUniqueName();
            }

            if (displayHash.containsKey(uniqueName)) {
                restoreDisplay((HistoryData)displayHash.get(uniqueName));
            } else {
                cleanDisplays();
            }
            //relationTable.getTable().setRowSelectionInterval(0, 0);
        }
    }

    /** Get the display data and made them shown on the screen. */
    public void restoreDisplay(HistoryData disData) {
        if(disData == null) {
            // no display
            searchResult.setText("");
            for(int i=0; i<displays.length; i++) {
	        displays[i].setData(null);
            }
        }

        searchResult.setText(disData.getTitle());
	Vector restoreData = disData.getStringArrayVector();
	for (int i=0; i<displays.length; i++) {
            String[] next = (String[])restoreData.elementAt(i);
	    displays[i].setData(next);
        }
    }

    /** Creates the left panel with the search button.  There are function
    *  calls made to create a search specification panel as well as a history
    *  panel.
    */
    private JComponent createSearchComponent(){
        JLabel word = new JLabel("Search Word:");
        Box box1 = Box.createHorizontalBox();
        box1.add(word);
        box1.add(Box.createHorizontalGlue());

        searchButton = new JButton("Search");
        searchButton.setToolTipText("Search for the Entered Text");
        searchButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent event) {
                if (searchField.isEditable()) {
                    // This is corresponding to the setBusy
                    String term = new String(searchField.getText());
                    if (!term.equals("")) {
                        doSearch(term);
                    }
                }
            }
	});

        searchField = new JTextField("");
        searchField.setPreferredSize(new Dimension(75, 20));

        JScrollPane scrollpane = new JScrollPane(searchField);
        scrollpane.setPreferredSize(new Dimension(80, 20));
        scrollpane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_NEVER);

        searchField.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent event) {
                String term = new String(searchField.getText());
                if (!term.equals("")) {
                    doSearch(term);
                }
            }
        });

	JPanel panel = new JPanel();
	JPanel search = new JPanel();
        search.setLayout(new BorderLayout());
	search.setBorder(BorderFactory.createRaisedBevelBorder());
	search.add(scrollpane,BorderLayout.NORTH);

        /**
        * Query the tab to see if there is a search specification panel that
        * should be added.  This will be implemented for the UMLS tab.
        */
	JPanel spec = getSearchSpecPanel();
        if (spec != null) {
            search.add(spec, BorderLayout.CENTER);
        }

        //adding the history panel
        search.add(createHistory(), BorderLayout.SOUTH);

        /**
         * Query the tab to see if there is a user-defined panel that should
         * be added.  This will be implemented for the UMLS tab.
        */
        panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
        panel.add(box1);

        JPanel mainPanel = new JPanel(new BorderLayout());
        mainPanel.add(search, BorderLayout.NORTH);

        JPanel panel7 = new JPanel();
        panel7.setBorder(BorderFactory.createRaisedBevelBorder());
        JComponent[] userComponents = getUserDefinedComponents();

        progress = new JProgressBar();
        panel7.add(progress);

        if (userComponents != null) {
            for (int i=0;i< userComponents.length;i++) {
                panel7.add(userComponents[i]);
            }
        }

        panel7.setLayout(new BoxLayout(panel7, BoxLayout.Y_AXIS));
        mainPanel.add(panel7, BorderLayout.CENTER);
	panel.add(mainPanel);
	return new JScrollPane(panel);
    }

    /**  This creates the history panel which allows caching of old search
     *   results.
     */
    private JComponent createHistory() {
        Icons allIcon = new Icons();
        backbutt = new JButton("<<");
	backbutt.setToolTipText("Back: Previous Search");
	backbutt.setPreferredSize(new Dimension(50,30));
	hisIndicator = new JLabel("  ");

        backbutt.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                unDo();
            }
	});

        forwardbutt = new JButton(">>");
	forwardbutt.setToolTipText("Forward: Next Search");
	forwardbutt.setPreferredSize(new Dimension(50,30));
	forwardbutt.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                reDo();
            }
        });

        JPanel panel5 = new JPanel();
        panel5.setLayout(new BoxLayout(panel5, BoxLayout.X_AXIS));

        panel5.add(backbutt);
        panel5.add(hisIndicator);
        panel5.add(forwardbutt);

        panel5.add(searchButton);

        JPanel panel2 = new JPanel();
        panel2.setLayout(new BoxLayout(panel2, BoxLayout.Y_AXIS));
        panel2.add(panel5);

        return panel2;

    }

    /** Creates the right most tabbed pane which has the relation panel on the
     *  left and ProtegeView on the right.
     */
    private JComponent createResultSplitter() {
        JSplitPane resultPane = createLeftRightSplitPane("Result.left_right", RESULTLEFT_DEFAULT);
	resultPane.setLeftComponent(createRelationsPanel());
	resultPane.setRightComponent(createProtegeView());
	return resultPane;
    }

    /** Get the RelationTable. */
    private JComponent createRelationTablePanel() {
        return relationTable.getComponent();
    }

    /** Get the RelationDisplay's calls initializeDisplay which initializes
     *  the displays and then creates the graphical display
     */
    private JComponent createRelationsPanel(){
        displays = createRelationDisplays();

        if (displays == null){
            displays = new RelationDisplay[0];
            return null;
        }

        addMouseListeners(displays);

        JPanel relPanel = new JPanel();
        relPanel.setLayout(new BoxLayout(relPanel, BoxLayout.Y_AXIS));

        Box outter = Box.createVerticalBox();
        searchResult = new JLabel(" ", JLabel.LEFT);
        searchResult.setPreferredSize(new Dimension(150, 40));
        searchResult.setFont(new Font("Dialog", Font.ITALIC,18));

        Box box1 = Box.createHorizontalBox();
        createButtons();

        box1.add(Box.createHorizontalGlue());
        box1.add(classButton);
        box1.add(instanceButton);

        relPanel.add(searchResult);
        relPanel.add(box1);

        relPanel.add(initializeDisplays(displays));

        return new JScrollPane(relPanel);
    }

    /** Create main menu to turn on/off relation display. */
    private void createMainMenuItem() {
        JMenuBar mainMenuBar = this.getMainWindowMenuBar();
        TabMenu tabMenu = new TabMenu();
        mainMenuBar.add(tabMenu);
    }

    /** Main menu system. */
    static class TabMenu extends JMenu{
        public TabMenu() {
            super("UMLSTab", true);
        }
    }


    /** The createbuttons function is used to create a class with the
     *  appropriate slots. The current result in the relation display will
     *  be added as the correspond slot values.
    */
    private void createButtons(){
        classButton = new JButton(">");
        classButton.setToolTipText("create class from current concept");
        Icon classIcon = Icons.getClsIcon();
        classButton.setIcon(classIcon);
        classButton.setPreferredSize(new Dimension(60,30));
        classButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                if (searchField.isEditable()) {
                    addClass(searchResult.getText());
                }
            }
        });

        /**  Create an instance. Repeat behavior of ARTab. */
        instanceButton = new JButton(">");
        instanceButton.setToolTipText("create instance from current concept");
        Icon instanceIcon = Icons.getInstanceIcon();
        instanceButton.setIcon(instanceIcon);
        instanceButton.setPreferredSize(new Dimension(60, 30));
        instanceButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e){
                if(searchField.isEditable()) {
                    addInstance(searchResult.getText());
                }
            }
        });
    }

    /** Add mouse listeners to relation display and relation table. */
    protected void addMouseListeners(RelationDisplay[] displays){
        comMouse = new CommonMouse();

        for (int i=0; i< displays.length; i++) {
            displays[i].getWidget().addMouseListener(comMouse);
        }

        if (hasTable()) {
            tablemouse = new TableMouse(relationTable);
            relationTable.getTable().addMouseListener(tablemouse);
        }
    }

    /** Define table mouse adapter. Single click will show the search result.
     *  Double click will start a new presearch. */
    protected class TableMouse extends java.awt.event.MouseAdapter
    {
        RelationTable relationTable;
        public TableMouse(RelationTable t) {
            relationTable = t;
        }

	public void mousePressed(java.awt.event.MouseEvent event) {
            Object object = event.getSource();
	    String item;
	    int index;

	    if (!(object instanceof JTable)) { return; }

            index = relationTable.getTable().getSelectedRow();
	    if (index < 0) { return; }
            if (relationTable.getTableModel().getRowCount() == 0) { return; }
            item = (String) relationTable.getTableModel().getValueAt(index,0);

            if (! searchField.isEditable()) { return; }
            if (event.getClickCount() == 2) {
                setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
		searchField.setText(item);
		doSearch(item);
		setCursor(Cursor.getDefaultCursor());
            }  else {
                Object relatedObject = getSearchSpecification();
                setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
		displaySearch(item, relatedObject);
		setCursor(Cursor.getDefaultCursor());
            }
        }
    }


    /** Add specified string as the class. */
    private void addClass(String className) {
        if (className.length() < 1) { return; }

        if (kb.containsFrame(className)) {
            clsFinder.doFind(className);
            clsFinder.repaint();
            return;
        }

        Cls currentSelection = getSelectedCls();
        if (currentSelection == null) currentSelection = kb.getRootCls();

        Collection parentcls = new ArrayList();
        parentcls.add(currentSelection);
        Cls firstcls = kb.createCls(className, parentcls);
        firstcls.setAbstract(false);

        addSlotValuesToFrame(className, firstcls, true);    // is a class
        addSlotsToClass(firstcls, getSlotNames(), false);   // is not metaclass
    }

    private void addInstance(String instanceName) {
        Cls selectedCls = this.getSelectedCls();
        if (selectedCls == null) {
            selectedCls = kb.getRootCls();
        }
        Instance newInstance = kb.createInstance(instanceName, selectedCls);
        addSlotValuesToFrame(instanceName, newInstance, false);
    }

    /** Add specified string as a slot value to the specified frame. */
    private void addSlotValuesToFrame(String name,
                 edu.stanford.smi.protege.model.Frame frame, boolean isClass) {
        if (isClass) {
            ModelUtilities.setOwnSlotValue(frame, ":NAME", name);
        } else {
            ModelUtilities.setOwnSlotValue(frame, "name", name);
        }

        HistoryData data = (HistoryData)displayHash.get(getUniqueName(name));
        Vector restoreData = data.getStringArrayVector();
        for (int i=1; i<displays.length; i++) {
            String slotName = displays[i-1].getSlotName();
            if (slotName == null) continue;
            if (slotName.equalsIgnoreCase(":Documentation") && !isClass) {
                slotName = "documentation";
            }

            String[] values = (String[])restoreData.elementAt(i-1);
            if (values == null) continue;
            boolean single = displays[i].isSlotSingleValued();
            if (single) {
                ModelUtilities.setOwnSlotValue(frame, slotName , values[0]);
            } else {
                ModelUtilities.setOwnSlotValues(frame, slotName, Arrays.asList(values));
            }
        }
    }

    /** Get name of slot. */
    private String[] getSlotNames(){
        String[] names = new String[displays.length];
        for (int i = 0; i < displays.length; i++) {
            names[i] = displays[i].getSlotName();
        }
        return names;
    }

    public RelationDisplay[] getDisplays() {
        return displays;
    }


    /** sets the tab and project for each display and then calls
    * initialized to signal that the display is ready.
    * Each RelationDisplay has a JComponent to be displayed on screen.
    * Then the graphical display is created (RelationPanel).
    * To change the setup behavior or the display, override
    * this method.  The set of slots to create for the class
    * is also collected here.
    */
    protected JComponent initializeDisplays(RelationDisplay d[]){
        Project p = getProject();
	JComponent[] comps = new JComponent[d.length];
        String[] titles = new String[d.length];
	String[] slots = new String[d.length];

	int i;
	for (i = 0; i < d.length; i++) {
            RelationDisplay rd = d[i];
            rd.setKBTab(this);
            rd.setProject(p);
            rd.initialized();
            comps[i] = rd.getComponent();
            titles[i] = rd.getLabel();
            slots[i] = rd.getSlotName();
        }

	createClass(slots);
        relationPanel = (RelationPanel) getRelationPanel(comps, titles);
	return  relationPanel;
    }

    /** Deep copy the vector. */
    protected void copyVec(Vector newvec,Vector oldvec) {
        newvec.clear();
        for (int i = 0; i<oldvec.size(); i++) {
            newvec.addElement(oldvec.elementAt(i));
        }
    }

    /**  This function can be overrided to create a new JPanel that
    * displays the array of JComponents.  The default is
    * a panel that displays each RelationDisplays in a separate tab of a
    * tabbed pane
    */
    protected JComponent getRelationPanel(JComponent[] c, String[] title) {
        return new RelationPanel(c, title, this);
    }

    /** Creates the set of slots this knowledge base supports. A default
    * meta class, and a root class are also created.
    */
    protected void createClass(String[] slotNames){
        Project project = getProject();

        String newName = getClassName();
	if (newName == null) {
            newName = project.getName();
        }

	Cls oldMetaCls = kb.getCls(":STANDARD-CLASS");
	kb.setDefaultClsMetaCls(oldMetaCls); // set the new default class

        Cls cls = kb.getCls(newName);
        if (cls == null) {
            Collection parentcls = new ArrayList();
            parentcls.add(oldMetaCls);
            cls = kb.createCls(newName,parentcls);
            kb.setDefaultClsMetaCls(cls); // set the new default class
            cls.setAbstract(false);       // set as the concrete class
            thisClass = cls;
	} else {
	    kb.setDefaultClsMetaCls(cls); // set the new default class
            cls.setAbstract(false);
	    thisClass = cls;
	}
        addSlotsToClass(cls, slotNames, true);

	// after the meta class is created, another root class for the
        // UMLS should be created
	Cls rootCls;
	String rootName = newName + "_ROOT";
	Cls root = kb.getRootCls();
	if (kb.getCls(rootName) == null) {
            Collection parentcls = new ArrayList();
            parentcls.add(root);
	    rootCls = kb.createCls(rootName,parentcls, cls);
	    rootCls.setAbstract(false); // set as the concrete class
        } else {
            rootCls = kb.getCls(rootName);
        }

        addSlotsToClass(rootCls, slotNames, false);
    }

    /** Add specified slots to specifed class. If it is meta, test it whether
     *  is the two special slots are used or not.  */
    private void addSlotsToClass(Cls cls, String[] slotNames, boolean isMeta){
        for (int i = 0; i < slotNames.length + 1; i++) {
            String name;
	    if (i != slotNames.length) {
                name = slotNames[i];
            } else {
                if (!isMeta) {
		    name = "name";
                } else {
                    name = ":NAME";
                }
            }

            if (name == null) continue;

            Slot slot;
            // check whether it is meta class or not.
            if (name.equalsIgnoreCase(":Documentation") && !isMeta) {
                name = "documentation";
            }
            slot = kb.getSlot(name);
	    if (slot == null) {
                slot = kb.createSlot(name);
                if (i != slotNames.length) {
                    slot.setAllowsMultipleValues(!displays[i].isSlotSingleValued());
                } else {
                    slot.setAllowsMultipleValues(false);
                }
            }

            if(cls.hasTemplateSlot(slot)) continue;
            cls.addDirectTemplateSlot(slot);
        }
    }


    /** Create the right panel that displays the classes and instances of the
     *  current Protege knowledge base.
     */
    private JComponent createProtegeView() {
        InstancesViewPanel instancesViewPanel = new InstancesViewPanel();
        ClsesViewPanel clsesViewPanel = new ClsesViewPanel();

        JTabbedPane instance_class = new JTabbedPane();
        instance_class.setPreferredSize(new Dimension(300,300));
        instance_class.addTab("Classes",null,clsesViewPanel,"Show Classes Tree");
        instance_class.addTab("Instances",null,instancesViewPanel,"Show Instances Tree");
        return instance_class;
    }


    /** This is the main method for performing a search. Each display is queried
    * in turn, the results are displayed, and saved in the history
    */
    public void doSearch(String term) {
        setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));

        String obj = getUniqueName(term);
	HistoryData historyData = new HistoryData();

        boolean searchMatch;
        historyData.setSearchSpecification(getSearchSpecification());
        if (hasTable()) {
            searchMatch = searchTable(historyData);
        } else {
            searchMatch = false;
            searchResult.setText(term);
        }
        doSearch(searchResult.getText().trim(), getSearchSpecification());
        searchField.setText(term);
        historyData.setSearchWord(term);
        String newObj = getUniqueName(term);
        historyData.setUniqueName(newObj);
        historyHash.put(historyData.getUniqueName(), historyData);
        addHistory(historyData.getUniqueName());
        hisIndicator.setText("" + (searchHistory.getHisPosition()+1) );

	setCursor(Cursor.getDefaultCursor());
    }

    /** Presearch is called and the presearch result is set into the table. */
    private boolean searchTable(HistoryData historyData) {
        // begin to search the word
        String word = (searchField.getText()).toLowerCase().trim();
        if(!hasTable()) return false;
        try {
            preSearch(word, historyData);
            searchResult.setText(relationTable.getSearchResultText());
            if (relationTable.getSearchResultText().toLowerCase().equals(word)) {
                return true;
            }
        } catch ( Exception e) {
	    System.out.println(e.getMessage());
            e.printStackTrace();
	}
	return false;
    }

    /** Do the presearch on the specified string. Set the result to the
     *  table. */
    protected Object preSearch(String targetStr, HistoryData historyData) {
        try {
            relationTable.search(targetStr);

            Vector tableVector = relationTable.getModelVector();
            historyData.setTableVector(tableVector);
            Vector objectVector = relationTable.getTableVecObj();
            historyData.setObjectVector(objectVector);

            relationTable.setData(tableVector, objectVector);
            if (tableVector!= null) {
                if (tableVector.size() > 0) {
                    relationTable.getTable().setRowSelectionInterval(0, 0);
                } else {
                    cleanDisplays();
                }
            }
        } catch(Exception e) {
            e.printStackTrace();
            cleanDisplays();
            return null;
        }

        Vector obj = relationTable.getTableVecObj();
        if (obj == null || obj.size() <= 0) {
            return null;
        } else {
            return obj.elementAt(0);
        }
    }



    /** Sets the searchField text to the newTerm, useful if the double
    * click actions in the RelationPanel need to perform a specialized
    * search.  In that case, call this search function with the necessary
    * newTerm and search context.
    */
    public void doSearch(String newTerm, Object context){
        searchField.setText(newTerm);
        /* Setting the text is often unnecessary since this doSearch is often called
        when the person presses the search button, but when searches are done
        by double clicking in one of the RelationDisplay's it will be necessary
        to update the searchField
        */
	displaySearch(newTerm, context);
    }

    /** Add the newTerm to the history record with the unique name. */
    public void addHistory(String newTerm) {
        String unique = getUniqueName(newTerm);
        searchHistory.addHisRecord(newTerm, unique);
        updateLabel();
    }

    /** Control the display of 'undo','redo' buttons under different
     *  conditions. */
    public void updateLabel() {
        if (searchHistory.getHisSize() < 2) {
            backbutt.setEnabled(false);
            forwardbutt.setEnabled(false);
        } else {
            if (searchHistory.getHisPosition() > 0) {
                backbutt.setEnabled(true);
            } else {
                backbutt.setEnabled(false);
            }

            if (searchHistory.getHisPosition() < searchHistory.getHisSize() - 1) {
                forwardbutt.setEnabled(true);
            } else {
                forwardbutt.setEnabled(false);
            }
        }
    }

    /** If the unique name can be found in the hashtable, get the search result
     *  and restore display. Otherwise, update the display hashtable according
     *  to the search result and them display them. */
    protected void displaySearch(String term, Object context){
        int completeNum = 0;
        searchTerm = term;
        searchObj = context;
        String obj = getUniqueName(term);
	if (displayHash.containsKey(obj)) {
	    HistoryData record = (HistoryData)displayHash.get(obj);
	    restoreDisplay(record);
        } else {
            HistoryData record = new HistoryData();
	    record.setTitle(term);
	    record.setUniqueName(obj);

	    initProgress(displays.length+1);
            //setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));

	    Vector displayRecord = new Vector();

            for(int i = 0; i < displays.length; i++) {
                String[] result = displays[i].search(term, context);
                try {
                    if (displays[i].isComplete()) {
                        completeNum++;
                    }
                } catch(Exception e) {
                    System.out.println(e.getMessage());
                }

                displayRecord.addElement(result);
		searchProgress();
		displays[i].setData(result);
            }

	    record.setStringArrayVector(displayRecord);

            // only push it to the Hash when everthing is done
            if ( completeNum == displays.length) {
                displayHash.put(obj, record);
                endProgress();
                setCursor(Cursor.getDefaultCursor());
            }

            searchResult.setText(term);
        }
    }

    public void displayUpdate() {
        int completeNum = 0;
        String term = searchTerm;
        String obj = getUniqueName(term);
	if (displayHash.containsKey(obj)) {
	    HistoryData record = (HistoryData)displayHash.get(obj);
	    restoreDisplay(record);
	} else {
            HistoryData record = new HistoryData();
	    record.setTitle(term);
	    record.setUniqueName(obj);

	    initProgress(displays.length);
	    Vector displayRecord = new Vector();

	    for (int i = 0; i < displays.length; i++) {
                String[] result = displays[i].getData();
                displayRecord.addElement(result);
		searchProgress();
		displays[i].setData(result);
            }

	    record.setStringArrayVector(displayRecord);
	    endProgress();
            displayHash.put(obj, record);
            searchResult.setText(term);
        }
    }

    protected void clearDisplays() {
        for(int i = 0; i < displays.length; i++) {
            String[] result = new String[1];
            result[0] = new String("");
            searchProgress();
            displays[i].setData(result);
        }
    }

    /** Clean the relation displays. */
    protected void cleanDisplays() {
        searchResult.setText("");
        for(int i = 0; i < displays.length; i++) {
            String[] result = new String[1];
            result[0] = new String("");
            displays[i].setData(result);
        }
    }

    /** If this tab is visible, set the new default metaclass. */
    public void setVisible(boolean visible) {
        if (visible) {
            kb.setDefaultClsMetaCls(thisClass);
        }
        // set the new default class
        super.setVisible(visible);
    }


    /** Returns either the class or instance that is currently selected and
     *  visible on screen.  If the Class panel is in front, then a class will
     *  be returned, otherwise an Instance.
    */
    public edu.stanford.smi.protege.model.Frame getSelectedFrame(){
        return currentSelectedFrame;
    }

    /** Returns the Instance that is selected in the ProtegeView. */
    public Instance getSelectedInstance(){
        return	currentSelectedInstance;
    }

    /** Returns the class that is selected in the ProtegeView. */
    public Cls getSelectedCls() {
        return currentSelectedCls;
    }

    /** Returns the class that is selected in the ProtegeView. */
    public Cls getSelectedCls2() {
        Collection selection = ComponentUtilities.getSelection(itsClsTree);
        Cls selectedClass = null;
        if (selection != null) {
            selectedClass = (Cls) CollectionUtilities.getFirstItem(selection);
        }
        return selectedClass;
    }

    public RemoteKBTabClsTreeFinder getClsFinder() {
        return clsFinder;
    }

    /** Inner class that shows the Protege instances. */
    class InstancesViewPanel extends JPanel
    {
        public InstancesViewPanel() {
            setupDragAndDrop();
            setComponents();
            add(createInstancesSplitter());
        }

        private JComponent createClsesWidget() {
            itsClsTree = ComponentFactory.createTree(null);
            itsClsTree.setModel(new LazyTreeModel(new ParentChildRoot(getKnowledgeBase().getRootCls())));
            FrameRenderer renderer = new FrameRenderer();
            renderer.setDisplayDirectInstanceCount(true);
            itsClsTree.setCellRenderer(renderer);
            itsClsTree.expandRow(0);

            itsClsTree.addTreeSelectionListener(new TreeSelectionListener() {
                public void valueChanged(TreeSelectionEvent event) {
                    Collection selection = ComponentUtilities.getSelection(itsClsTree);
                    Collection itsClsestmp = Collections.EMPTY_LIST;
                    itsClsestmp = new ArrayList(selection);
                    Iterator i = itsClsestmp.iterator();
                    while(i.hasNext()) {
                        Cls cls = (Cls)i.next();
                        currentSelectedFrame = currentSelectedCls = cls;
                    }
                    itsDirectInstancesList.setClses(selection);
                }
            });

            LabeledComponent c = new LabeledComponent("Classes", new JScrollPane(itsClsTree));
            c.setFooterComponent(new ClsTreeFinder(getKnowledgeBase(), itsClsTree, "Find Class"));
            return c;
        }

        private JComponent createInstancesSplitter() {
            JSplitPane clsesSplitter = createLeftRightSplitPane("Instance.left_right", INSTANCELEFT_DEFAULT);
            clsesSplitter.setLeftComponent(createClsesWidget());
            clsesSplitter.setRightComponent(createInstancesWidget());
            return clsesSplitter;
        }

        private void setComponents() {
            setLayout(new BoxLayout(this,BoxLayout.Y_AXIS));
        }

        private void setupDragAndDrop() {
            DragSource.getDefaultDragSource().createDefaultDragGestureRecognizer(itsDirectInstancesList,
            DnDConstants.ACTION_COPY_OR_MOVE, new InstancesListDragSourceListener());
            new DropTarget(itsClsTree, DnDConstants.ACTION_COPY_OR_MOVE, new InstanceClsesTreeTarget());
        }

        class InstancesListDragSourceListener extends ListDragSourceListener {

            public void doMove(JComponent c, int[] indexes, Collection objects) {
            }

            public void doCopy(JComponent c, int[] indexes, Collection objects){
            }
        }

        class InstanceClsesTreeTarget extends TreeTarget {
            public InstanceClsesTreeTarget() {
                super(false);
            }

            public boolean doDrop(JTree tree, Object source, int row, Object area) {
                boolean succeeded = false;
                TreePath path = tree.getPathForRow(row);
                Cls targetCls = (Cls) ((LazyTreeNode)path.getLastPathComponent()).getUserObject();
                Instance sourceInstance = (Instance) source;

                if (targetCls.isAbstract()) {
                    // do nothing
                } else if (sourceInstance.hasDirectType(targetCls)) {
                    Log.trace("do nothing", this, "doDrop", source, targetCls);
                } else {
                    if (sourceInstance instanceof Cls) {
                        if (targetCls.isClsMetaCls()) {
                            sourceInstance.setDirectType(targetCls);
                            succeeded = true;
                        }
                    } else if (!targetCls.isClsMetaCls()) {
                        sourceInstance.setDirectType(targetCls);
                        succeeded = true;
                    }
                }
                return succeeded;
            }
        }

        /** Updates the buttons to be enabled when the appropriate tab in the
        *  ProtegeView is visible.
        */
        public void setVisible(boolean visible) {
            if (visible) {
                classButton.setEnabled(false);
                instanceButton.setEnabled(true);
                currentSelectedFrame =  currentSelectedInstance;
                // disable the add class buttons
                for ( int i=0; i<classActions.size(); i++) {
                    if (classActions.elementAt(i) instanceof AbstractRemoteKBAction) {
                        ((AbstractRemoteKBAction) (classActions.elementAt(i))).setEnabled(false);
                    } else if (classActions.elementAt(i) instanceof JComponent) {
                        ((JComponent) (classActions.elementAt(i))).setEnabled(false);
                    }
                }

                // enable the add instance buttons
                for ( int i=0; i<instanceActions.size(); i++) {
                    if(classActions.elementAt(i) instanceof AbstractRemoteKBAction) {
                        ((AbstractRemoteKBAction) (instanceActions.elementAt(i))).setEnabled(true);
                    } else if( classActions.elementAt(i) instanceof JComponent) {
                        ((JComponent) (instanceActions.elementAt(i))).setEnabled(true);
                    }
                }
            }
            super.setVisible(visible);
        }

        private JComponent createInstancesWidget() {
            itsDirectInstancesList = new DirectInstancesList(getProject());
            itsDirectInstancesList.addSelectionListener(new SelectionListener() {
                public void selectionChanged(SelectionEvent event) {
                    Collection selection = itsDirectInstancesList.getSelection();
                    Instance selectedInstance;
                    if (selection.size() == 1) {
                        selectedInstance = (Instance) CollectionUtilities.getFirstItem(selection);
                    } else {
                        selectedInstance = null;
                    }
                    currentSelectedFrame = currentSelectedInstance = selectedInstance;
                }
            });
            return itsDirectInstancesList;
        }

    } //end InstancesViewPanel


    /**Inner class that shows the classes of the current Protege
     * Knowledge Base. */
    class ClsesViewPanel extends JPanel {
        private RemoteKBTabClsesPanel itsClsesPanel;
        public ClsesViewPanel() {
            setComponents();
            add(createClsesPanel());
        }

        /** Updates the buttons to be enabled when the appropriate tab in
         *  the ProtegeView is visible.
        */
        public void setVisible(boolean visible) {
            if (visible) {
                classButton.setEnabled(true);
                instanceButton.setEnabled(false);
                currentSelectedFrame = currentSelectedCls;

                // enable the add class buttons
                for (int i=0; i<classActions.size(); i++) {
                    if (classActions.elementAt(i) instanceof AbstractRemoteKBAction) {
                        ((AbstractRemoteKBAction) (classActions.elementAt(i))).setEnabled(true);
                    } else if (classActions.elementAt(i) instanceof JComponent) {
                        ((JComponent) (classActions.elementAt(i))).setEnabled(true);
                    }
                }

                // disable the add instance buttons
                for (int i=0; i<instanceActions.size(); i++) {
                    if (classActions.elementAt(i) instanceof AbstractRemoteKBAction) {
                        ((AbstractRemoteKBAction) (instanceActions.elementAt(i))).setEnabled(false);
                    } else if (classActions.elementAt(i) instanceof JComponent) {
                        ((JComponent) (instanceActions.elementAt(i))).setEnabled(false);
                    }
                }
            }
            super.setVisible(visible);
        }

        private void setComponents() {
            setLayout(new BoxLayout(this,BoxLayout.Y_AXIS));
        }

        private JComponent createClsesPanel() {
            itsClsesPanel = new RemoteKBTabClsesPanel(getProject());
            clsFinder = itsClsesPanel.getClsFinder();
            itsClsesPanel.addSelectionListener(new SelectionListener() {
                public void selectionChanged(SelectionEvent event) {
                    Cls selection = (Cls) transmitSelection();
                    currentSelectedFrame = currentSelectedCls = selection;
                }
            });
            return itsClsesPanel;
        }

        private Cls transmitSelection() {
            Collection selection = itsClsesPanel.getSelection();
            Cls selectedCls;
            if (selection.size() == 1) {
                selectedCls = (Cls) CollectionUtilities.getFirstItem(selection);
            } else {
                selectedCls = null;
            }
            return selectedCls;
        }
    }//end ClsesViewPanel

    /**Inner class that set the common mouse adapter. When the item in a list
     * receive double click, a new search will be initiated. */
    class CommonMouse extends java.awt.event.MouseAdapter {
        public void mousePressed(java.awt.event.MouseEvent event) {
            Object object = event.getSource();
            String[] items;

            if (!(object instanceof JList)) {
                return;
            } else {
                Object[] selected = ((JList)object).getSelectedValues();
		if (selected == null) { return; }
		if (selected.length < 1) { return; }

                String item = (String)selected[0].toString();
                if(event.getClickCount() == 2) {
                        searchField.setText( item);
                        doSearch( item);
                }
            }
	}
    }// end CommonMouse

    /********************************************************************
    * Methods that subclasses will most likely want to override.
    *********************************************************************/


    /**This is used when the tab needs to adjust the search specification,
    * such as for the UMLS tab.  Combo boxes of sources or something of the
    * kind.
    */
    protected JPanel getSearchSpecPanel(){
        return null;
    }

    /** This is used when the user need add their own components on the left
    *  hand side, such as for the UMLS tab, table is added to show the first
    *  type search.
    */
    protected JComponent[] getUserDefinedComponents() {
        if ((relationTable = getRelationTable()) != null) {
            JComponent[] comps;
            comps = new JComponent[2];
            JLabel label = new JLabel("Search Result:");
            comps[0] = label;
            comps[1] = createRelationTablePanel();
            initializeTable(relationTable.getTableLabels());
            return comps;
        } else {
            return null;
        }
    }

    /** Set the labels for the table. */
    protected void initializeTable(String[] tableLabels) {
        if (tableLabels == null) {
            return;
        } else {
            for (int i=0; i<tableLabels.length; i++) {
                relationTable.getTableModel().addColumn(tableLabels[i]);
            }
        }
    }

    /** Return the lower case of the specifed string. */
    protected String getUniqueName(String targetStr){
        return (targetStr == null ? null: targetStr.toLowerCase());
    }

    /** GetSearchSpecification() is called once for each search.
    * It is assumed that the context does not change between each search called
    * on the array of RelationDisplay's.   This might be the search
    * specifications such as in the UMLS tab.
    */
    public Object getSearchSpecification(){
        return null;
    }

    /* called when moving through the cached results in the history
     * to update the display as necessary.  A common case is that the
     * search specificiation panels will need to be reset.
     */
    protected void setFromHistory(Object userdata){
    }

    /** Get class name. */
    protected String getClassName(){
        return null;
    }

    /** Create relation displays in the tab. */
    abstract protected RelationDisplay[] createRelationDisplays();

    /** Get the relationtable. */
    abstract public RelationTable getRelationTable();

    /**  Setup is the signal to the subclasses of RemoteKBTab that they
     *   should begin creating there search specification and RelationDisplay's.
     *   It will be called sometime after initialize has been called on this
     *   tab. */
    abstract public void setup();

    /** getDisplayObject is used to return current display Object. */
    public Object getDisplayObject() {
        return null;
    }

    /** get specification components */
    public Vector getSpecComponents() {
        return null;
    }

    /** Setup the configure panel */
    public boolean configure() {
        return false;
    }

    /*public static void main(String[] args) {
        edu.stanford.smi.protege.Application.main(args);
    }*/
}