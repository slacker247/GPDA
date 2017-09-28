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

import javax.swing.*;
import java.util.*;
import java.awt.*;
import java.awt.event.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.util.*;
import edu.stanford.smi.protege.resource.*;
import edu.stanford.smi.protege.ui.*;
import edu.stanford.smi.protegex.queries_tab.toolbox.*;

public class SearchWidget extends JPanel {
  private final int mWIDTH = 580;
  private final int mHEIGHT = 70;
  private KnowledgeBase itsKB;
  private QueriesTab itsTab;
  private SlotsModel slotsModel;
  private ConstraintsModel constraintsModel;

  private ClassSelectWidget searchClass;
  private SlotSelectWidget searchSlot;

  private JComboBox searchMethod;
  private JComponent searchFlow;
  private JCheckBox checkbox;
  protected int searchStyle;

  private SlotValueWidget searchObject;
  private Vector searchObjects;           // a vector of slotvaluewidget
  private int activeIndex;                // indicate which one is be shown

  private boolean isReady;                // whether the input is ready

  private Cls selectCls;
  private String selectSlotName;

  private JPanel mainParts;

  private LabeledComponent searchS;

  private String[] types = {"ANY","BOOLEAN","CLS", "FLOAT", "INSTANCE", "INTEGER", "STRING", "SYMBOL"};

    public SearchWidget(QueriesTab tab, KnowledgeBase kb) {
        super();
        itsTab = tab;
        itsKB = kb;
        this.setPreferredSize(new Dimension(690, 70));
        slotsModel = new SlotsModel(null, null, null);
        constraintsModel = new ConstraintsModel();
        initialize();
    }

    public SearchWidget(QueriesTab tab, KnowledgeBase kb, String[] slotNames, String[] slotTypes, Collection slotCol) {
        super();
        itsTab = tab;
        itsKB = kb;
        slotsModel = new SlotsModel(slotNames, slotTypes, slotCol);
        constraintsModel = new ConstraintsModel();
        initialize();
    }

    public void clearSearch() {
        setClass(null);
        searchSlot.setDisplayedSlot(null);
        searchStyle = 1;
        checkbox.setSelected(true);
    }

    public void clearSearchObjectContent(String name) {
        String[] slotNames = new String[1];
        slotNames[0] = name;

        String searchObjectLabel = searchObject.getLabel();
        if (searchObjectLabel.equalsIgnoreCase("integer") || searchObjectLabel.equalsIgnoreCase("float"))
            searchObject.setData(null);
        else
            searchObject.setData(slotNames);

    }

    private Collection combineCollection(Collection col1, Collection col2) {
        Collection resultInstances = col1;
        Collection tmpInstances = (Collection) col2;
        if (tmpInstances == null)
            return resultInstances;
        Iterator j = tmpInstances.iterator();
        while (j.hasNext()) {
            Instance tmpInstance = (Instance) j.next();
            if (resultInstances.contains(tmpInstance))
                continue;
            else
                resultInstances.add(tmpInstance);
        }
        return resultInstances;
    }

    private boolean containSlot(String[] names, String slotName) {
        for (int i = 0; i < names.length; i++) {
            if (names[i].equalsIgnoreCase(slotName))
                return true;
        }
        return false;
    }

    public void dispose() {
        searchClass.removeListener();
        searchSlot.removeListener();
        for (int i = 0; i < searchObjects.size(); i++) {
            ((SlotValueWidget) searchObjects.elementAt(i)).removeListener();
        }
    }

    public void enableQueryButton(boolean b) {
        for (int i = 0; i < searchObjects.size(); i++) {
            if (searchObjects.elementAt(i) instanceof InstanceSlotValueWidget) {
                ((InstanceSlotValueWidget) searchObjects.elementAt(i)).enableQueryButton(b);
                break;
            }
        }
    }

    public JComponent flowWidgets() {
        JPanel panel = new JPanel();
        panel.setLayout(new FlowLayout());

        for (int i = 0; i < searchObjects.size(); i++) {
            panel.add(((SlotValueWidget) searchObjects.elementAt(i)).getWidget());
        }
        setWidget(1); // make the first one visible
        return panel;

    }

    public int getAddStyle() {
        return searchStyle;
    }

    /** return the specified cls in this searchWidget. */
    public Cls getCls() {
        return selectCls;
    }

    // get the direct or subclasses instances
    private Collection getClsInstance(Cls cls, int type) {
        Collection resultInstances = new ArrayList();
        if (type == 0)
            resultInstances = cls.getDirectInstances();
        else
            resultInstances = cls.getInstances();
        return resultInstances;
    }

    public String getDisplayedSlotName() {
        return searchSlot.getSlotName();
    }

    // get the suitable instances from the selected class
    public Collection getInstances(Cls cls) {

        Collection resultInstances = null;
        int type = getAddStyle();
        Slot slot = null;
        if (cls == null) {
            String currentSlot = getSlotName();
            if (currentSlot == null || currentSlot.length() < 1)
                return null;
            slot = itsKB.getSlot(currentSlot);
        }
        return getInstances(cls, type, slot);
    }

    public Collection getInstances(Cls cls, int type, Slot slot) {
        Collection resultInstances = null;
        if (cls != null) {
            resultInstances = getClsInstance(cls, type);
            if (!isReady()) {
                setReady(isReady);
                return null;
            }
        } else {
            if (slot == null)
                return null;
            Collection clses = slot.getTemplateSlotClses();
            Iterator j = clses.iterator();
            int count = 0;
            while (j.hasNext()) {
                if (count == 0) {
                    resultInstances = getClsInstance((Cls) j.next(), type);
                    count++;
                    continue;
                }
                Collection tmpInstances = getClsInstance((Cls) j.next(), type);
                resultInstances = combineCollection(resultInstances, tmpInstances);
            }
        }
        return (Collection) resultInstances;
    }

    public KnowledgeBase getKB() {
        return itsKB;
    }

    public SlotValueWidget getMatchSlotWidget(String slotType) {
        for (int i = 0; i < searchObjects.size(); i++) {
            SlotValueWidget widget = (SlotValueWidget) searchObjects.elementAt(i);
            if (widget.getLabel().equalsIgnoreCase(slotType)) {
                return widget;
            }
        }
        return null;
    }

    public String getQueryName() {
        if (searchObject.getSelectedObject() instanceof InstancesQuery)
            return ((InstancesQuery) searchObject.getSelectedObject()).getName();
        return null;
    }

    public ClassSelectWidget getSearchClass() {
        return searchClass;
    }

    public String getSearchConstraint() {
        return (String) searchMethod.getSelectedItem();
    }

    public SlotValueWidget getSearchObject() {
        return (SlotValueWidget) searchObject;
    }

    // This is equal to get the slotname
    public String getSearchSubject() {
        if (searchSlot == null || searchSlot.getData() == null)
            return null;
        String name = (String) searchSlot.getData()[0];

        if (name == null)
            return null;
        return name;
    }

    /** Get the selected Objects for the Query. */
    public Object[] getSelectedObjects() {
        Object[] objs = new Object[5];

        objs[0] = searchClass.getSelectedObject();
        objs[1] = searchSlot.getSelectedObject();
        objs[2] = searchMethod.getSelectedItem();
        objs[3] = searchObject.getSelectedObject();
        String checkStatus = new String();
        if (checkbox.isSelected())
            checkStatus = "1";
        else
            checkStatus = "0";
        objs[4] = checkStatus;

        return objs;
    }

    // This is different from the getSlotName in that it gets the original settings.
    public String getSelectSlotName() {
        return selectSlotName;
    }

    private String getSlotName() {
        return getSearchSubject();
    }

    public SlotsModel getSlotsModel() {
        return slotsModel;
    }

    /** return type for the selected subject */
    public String getSlotType() {
        int index = 0;
        String name;

        if (searchSlot.getData() == null)
            return null;
        name = (String) searchSlot.getData()[0];
        if (name == null)
            return null;

        for (int i = 0; i < slotsModel.getSlotNames().length; i++) {
            if (slotsModel.getSlotNames()[i].equals(name))
                index = i;
        }
        String type = slotsModel.getSlotTypes()[index];
        return type;
    }

    public QueriesTab getTab() {
        return itsTab;
    }

    public Object getTargetValue() {
        if (searchObject.getData() == null || searchObject.getData()[0] == null)
            return null;
        return searchObject.getData()[0];
    }

    public JComponent getValueWidgets(int i) {
        searchObject = (SlotValueWidget) searchObjects.elementAt(i);
        return ((SlotValueWidget) searchObjects.elementAt(i)).getWidget();
    }

    private void initialize() {
        searchObjects = new Vector();
        activeIndex = -1;
        isReady = true;
        searchStyle = 1; // default: search with children

        setComponents();
    }

    /** used to initialize Symbol and instance slot target values. */
    private void initializeSubject(String type) {
        if (type.equalsIgnoreCase("symbol")) {
            // get the fixed value for this symbol slot.
            //Slot slot = selection.getProject().getKnowledgeBase().getSlot(type.toLowerCase());
        }
        return;
    }

    public boolean isReady() {
        return isReady;
    }

    /** Load the slot values to combobox */
    private boolean loadSlots(Cls cls) {
        boolean isContained;
        Object methodItem = searchMethod.getSelectedItem();
        Object[] objectItem = searchObject.getData();

        int nSlots;
        String slotsStr[];
        String slotsType[];
        Collection slots;

        Iterator i;

        if (cls != null) {
            nSlots = cls.getTemplateSlots().size();
            slotsStr = new String[nSlots];
            slotsType = new String[nSlots];
            slots = cls.getTemplateSlots();
        } else {
            Collection wholeInstances = itsKB.getSlots();

            nSlots = wholeInstances.size();
            slotsStr = new String[nSlots];
            slotsType = new String[nSlots];
            slots = wholeInstances;
        }

        i = slots.iterator();

        int j = 0;
        while (i.hasNext()) {
            Slot slot = (Slot) i.next();
            slotsStr[j] = slot.getName();
            slotsType[j] = Helper.getSlotType(slot);
            j++;
        }

        SlotsModel widgetSlotModel = getSlotsModel();
        String type = null;

        widgetSlotModel.initialize(slotsStr, slotsType, slots);
        String[] names = widgetSlotModel.getSlotNames();

        if (containSlot(names, getSelectSlotName())) {
            widgetSlotModel.setSelectedItem(getSelectSlotName()); //return;
            searchMethod.setSelectedItem(methodItem);
            isContained = true;
        } else {
            searchSlot.setDisplayedSlot(null);
            isContained = false;
        }

        return isContained;
    }

    public void removeMouseListener() {
        ((SlotValueWidget) searchClass).removeMouse();
        ((SlotValueWidget) searchSlot).removeMouse();
        for (int i = 0; i < searchObjects.size(); i++) {
            ((SlotValueWidget) searchObjects.elementAt(i)).removeMouse();
        }
    }

    /** Search method in the searchWidget. */
    public Collection search() {
        Collection instances;
        instances = getInstances(selectCls);
        return search(instances);
    }

    public Collection search(Collection instances) {
        if (instances == null)
            return null;
        SlotValueWidget currentWidget = getSearchObject();
        SlotSpecification specification = new SlotSpecification();

        specification.setName(getSearchSubject());
        specification.setType(getSlotType());

        specification.setConstraint(getSearchConstraint());
        specification.setValue(getTargetValue());

        currentWidget.setSpecification(specification);
        currentWidget.setCls(selectCls);

        currentWidget.setInstances(instances);
        return currentWidget.search();
    }

    /** This is used to set whole class */
    public void setClass(Instance instance) {

        selectCls = (Cls) instance;
        loadSlots(selectCls);
        setData(selectCls);
    }

    // This is used to set partial class except the cls for classSelectWidget.
    public void setClass2(Instance instance) {
        selectCls = (Cls) instance;
        if (!loadSlots(selectCls)) {
            setData2(selectCls);
        }

    }

    private void setComponents() {
        this.setLayout(new BorderLayout(10, 10));

        mainParts = new JPanel();
        mainParts.setLayout(new GridLayout(1, 4, 3, 3));
        setSize(mWIDTH, mHEIGHT);

        searchClass = new ClassSelectWidget(this);
        searchClass.setProject(itsKB.getProject());

        selectCls = null;

        checkbox = new JCheckBox("Subclasses", true);

        checkbox.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                if (e.getStateChange() == ItemEvent.SELECTED) {
                    searchStyle = 1; // search with children
                } else {
                    searchStyle = 0; // search without children
                }
            }
        });

        searchMethod = new JComboBox(constraintsModel);
        searchMethod.setPreferredSize(new Dimension(120, 25));

        // create integerslotvaluewidget
        SlotValueWidget searchObject1 = new IntegerSlotValueWidget(this);
        searchObjects.addElement(searchObject1);

        // create floatslotvaluewidget
        SlotValueWidget searchObject2 = new FloatSlotValueWidget(this);
        searchObjects.addElement(searchObject2);

        // create stringslotvaluewidget
        SlotValueWidget searchObject3 = new StringSlotValueWidget(this);
        searchObjects.addElement(searchObject3);

        // create stringslotvaluewidget
        SlotValueWidget searchObject4 = new SymbolSlotValueWidget(this);
        searchObjects.addElement(searchObject4);

        // create stringslotvaluewidget
        SlotValueWidget searchObject5 = new InstanceSlotValueWidget(this);
        searchObjects.addElement(searchObject5);

        SlotValueWidget searchObject6 = new BooleanSlotValueWidget(this);
        searchObjects.addElement(searchObject6);

        SlotValueWidget searchObject7 = new ClsSlotValueWidget(this);
        searchObjects.addElement(searchObject7);

        searchFlow = getValueWidgets(1);

        searchSlot = new SlotSelectWidget(this);
        searchSlot.setProject(itsKB.getProject());

        LabeledComponent searchM = new LabeledComponent("   ", searchMethod);
        mainParts.add(searchClass.getWidget());

        mainParts.add(searchSlot.getWidget());
        mainParts.add(searchM);

        mainParts.add(searchFlow);

        JPanel panel = new JPanel();
        panel.setLayout(new BorderLayout(10, 10));
        panel.add(mainParts, BorderLayout.CENTER);
        //panel.add(checkbox, BorderLayout.EAST);

        add(panel, BorderLayout.NORTH);

        // This can only been added after the initialization
        setClass(selectCls);
        searchClass.setCls(selectCls);

    }

    /** Set the suitable data for each slotvaluewidget. */
    public void setData(Cls cls) {
        if (cls == null) {
            ((ClassSelectWidget) searchClass).setCls(cls);
        }

        for (int i = 0; i < searchObjects.size(); i++) {
            ((SlotValueWidget) searchObjects.elementAt(i)).setCls(cls);
        }

        String currentSlot = getSlotName();
        if (currentSlot != null)
            updateSearchWidget(currentSlot);
    }

    /** Set the suitable data for each slotvaluewidget. */
    public void setData2(Cls cls) {
        if (cls == null) {
        }

        String currentSlot = getSlotName();

        for (int i = 0; i < searchObjects.size(); i++) {
            ((SlotValueWidget) searchObjects.elementAt(i)).setCls(cls);
        }

        if (currentSlot != null)
            updateSearchWidget(currentSlot);
    }

    public void setObjectsViewEnabled(boolean b) {
        for (int i = 0; i < searchObjects.size(); i++) {
            ((SlotValueWidget) searchObjects.elementAt(i)).setViewEnabled(b);
        }
    }

    public void setReady(boolean b) {
        isReady = b;
    }

    public void setSelectedObjects(Object[] objs, String[] names) {
        if (objs[0] != null) {
            if (itsKB.getCls(((Cls) objs[0]).getName()) != null)
                searchClass.setCls((Cls) objs[0]);
        }

        searchSlot.setDisplayedSlot((Slot) objs[1]);
        searchMethod.setSelectedItem(objs[2]);

        if ((Slot) objs[1] != null) {
            if (!(objs[3] instanceof Instance) && !(objs[3] instanceof InstancesQuery))
                searchObject.setSelectedObject(objs[3]);
            else if ((objs[3] instanceof Instance) && itsKB.getFrame(((Instance) objs[3]).getName()) != null)
                searchObject.setSelectedObject(itsKB.getFrame(((Instance) objs[3]).getName()));
            else if ((objs[3] instanceof InstancesQuery) && itsTab.getModel().getQuery((InstancesQuery) objs[3]) != null)
                searchObject.setSelectedObject(itsTab.getModel().getQuery((InstancesQuery) objs[3]));
        }

        if (objs[3] != null)
             ((SlotValueWidget) searchObject).setActionsEnabled(true);

        if (((String) objs[4]) == null || ((String) objs[4]).equals("1")) {
            searchStyle = 1;
            checkbox.setSelected(true);
        } else {
            searchStyle = 0;
            checkbox.setSelected(false);
        }

        if ((objs[0] == null && names[0] != null) || (objs[0] != null && itsKB.getCls(((Cls) objs[0]).getName()) == null)) {
            searchClass.setDisplayName(names[0]);
        }

        if (objs[1] == null && names[1] != null) {
            searchSlot.setDisplayName(names[1]);
        }

        if ((objs[3] == null && names[2] != null)
            || (objs[3] instanceof Instance && itsKB.getFrame(((Instance) objs[3]).getName()) == null)) {
            searchObject.setDisplayName(names[2]);
            ((SlotValueWidget) searchObject).setActionsEnabled(false);
        }

        if ((objs[3] == null && names[2] != null)
            || (objs[3] instanceof InstancesQuery)
            && itsTab.getModel().getQuery((InstancesQuery) objs[3]) == null) {
            searchObject.setDisplayName(names[2]);
            ((SlotValueWidget) searchObject).setActionsEnabled(false);
        }

    }

    public void setSelectSlotName() {

        selectSlotName = getSearchSubject();
    }

    public void setSize(int width, int height) {
        mainParts.setPreferredSize(new Dimension(width, height));
    }

    /** used to decide which widget should be used. */
    private void setUpWidget(String slotType) {

        if (slotType.equals("null")) {
            activeIndex = 2;
            updateWidget(2);
            return;
        }

        for (int i = 0; i < searchObjects.size(); i++) {
            SlotValueWidget widget = (SlotValueWidget) searchObjects.elementAt(i);
            if (widget.getLabel().equalsIgnoreCase(slotType)) {
                activeIndex = i;
                updateWidget(i);
                return;
            }
        }
    }

    public void setViewEnabled(boolean b) {
        ((SlotValueWidget) searchClass).setViewEnabled(b);
        ((SlotValueWidget) searchSlot).setViewEnabled(b);
        setObjectsViewEnabled(b);
    }

    /** Setup which one is displayed. */
    public void setWidget(int index) {
        for (int i = 0; i < searchObjects.size(); i++) {
            JComponent c = (JComponent) (((SlotValueWidget) searchObjects.elementAt(i)).getWidget());
            if (i == index) {
                c.setVisible(true);
                //c.enable(true);
                c.setEnabled(true);
                searchObject = (SlotValueWidget) searchObjects.elementAt(i);
            } else {
                c.setVisible(false);
                //c.enable(false);
                c.setEnabled(false);
            }
        }
    }

    /** Update the include children checkbox. */
    private void updateCheckBox(Cls cls) {
        if (cls.isAbstract()) {
            checkbox.setSelected(true);
            checkbox.setEnabled(false);
            searchStyle = 1;
        } else {
            if (checkbox.isEnabled())
                return;
            checkbox.setEnabled(true);
        }
    }

    /** Update the corresponding search methods and the slotvaluewidgets. */
    public void updateSearchWidget(String currentName) {
        String[] names = slotsModel.getSlotNames();
        String name = currentName;
        int m = -1;
        for (int i = 0; i < names.length; i++) {
            if (((String) names[i]).equalsIgnoreCase(name)) {
                m = i;
                break;
            }
        }

        if (m < 0) {
            constraintsModel.setUpComboBox("null");
            setUpWidget("null");
        } else {
            String tmpSlotType = slotsModel.getSlotTypes()[m];
            if (m > -1)
                constraintsModel.setUpComboBox(tmpSlotType);

            setUpWidget(tmpSlotType);
            initializeSubject(tmpSlotType);
        }

        String[] slotNames = new String[1];
        slotNames[0] = name;
        String searchObjectLabel = searchObject.getLabel();

        String currentSlot = getSlotName();
        if (currentSlot == null || currentSlot.length() < 1) {

            if (searchObjectLabel.equalsIgnoreCase("integer") || searchObjectLabel.equalsIgnoreCase("float"))
                searchObject.setData(null);
            else
                searchObject.setData(slotNames);
        }

    }

    /** Setup which one is displayed. */
    public void updateWidget(int index) {

        mainParts.remove(searchObject.getWidget());
        mainParts.add(getValueWidgets(index));
    }
}
