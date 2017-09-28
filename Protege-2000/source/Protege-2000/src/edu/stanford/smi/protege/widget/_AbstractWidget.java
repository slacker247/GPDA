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

package edu.stanford.smi.protege.widget;

import java.awt.*;
import java.awt.event.*;
import java.beans.*;
import java.util.*;
import javax.swing.*;
import javax.swing.border.*;
import javax.swing.event.*;
import edu.stanford.smi.protege.*;
import edu.stanford.smi.protege.event.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.model.Frame;
import edu.stanford.smi.protege.ui.*;
import edu.stanford.smi.protege.util.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public abstract class _AbstractWidget extends JComponent {
    private WidgetDescriptor _descriptor;
    private boolean _isDesignTime;
    private Project _project;
    private Cls _cls;
    private Slot _slot;
    private Cls _associatedCls;
    private ListenerCollection _selectionListeners = new ListenerList(new SelectionEventDispatcher());
    private ListenerCollection _widgetListeners = new ListenerList(new WidgetEventDispatcher());
    private String _label;
    private String _shortDescription;
    private Icon _icon;
    private Instance _instance;
    private boolean _isBusy;
    private int _preferredColumns = 0;
    private int _preferredRows = 0;
    private Collection _buttonInfo;
    private Collection _splitPanes;

    private FrameListener _instanceListener = new FrameAdapter() {
        public void ownSlotValueChanged(FrameEvent event) {
            handleOwnSlotValueChanged(event.getSlot());
        }
        public void browserTextChanged(FrameEvent event) {
            handleBrowserTextChanged();
        }
    };

    private FrameListener _slotListener = new FrameAdapter() {
        public void browserTextChanged(FrameEvent event) {
            handleFrameNameChanged();
        }
    };
    private ClsListener _associatedClsListener = new ClsAdapter() {
        public void templateFacetValueChanged(ClsEvent event) {
            if (event.getFacet() == _slot.getAssociatedFacet()) {
                loadValues();
            }
        }
    };

    private class ButtonInfo {
        public Action action;
        public boolean defaultState;
        public ButtonInfo(Action action, boolean defaultState) {
            this.action = action;
            this.defaultState = defaultState;
        }
    }

    public _AbstractWidget() {
        setLayout(new BorderLayout());
    }

    public void addButtonConfiguration(Action action) {
        addButtonConfiguration(action, true);
    }

    public void addButtonConfiguration(Action action, boolean defaultState) {
        if (_buttonInfo == null) {
            _buttonInfo = new ArrayList();
        }
        _buttonInfo.add(new ButtonInfo(action, defaultState));
        recordDefault(action, defaultState);
        String configuredDescription = getButtonDescription(action);
        if (configuredDescription != null) {
            action.putValue(Action.SHORT_DESCRIPTION, configuredDescription);
        }
    }

    public JButton addMainWindowToolBarButton(Action action) {
        JButton button;
        ProjectToolBar toolBar = getMainWindowToolBar();
        if (toolBar == null) {
            Log.warning("Cannot find window tool bar", this, "addMainWindowToolBarButton", action);
            button = null;
        } else {
            button = ComponentFactory.addToolBarButton(toolBar, action);
        }
        return button;
    }

    public void addSelectionListener(SelectionListener listener) {
        _selectionListeners.add(this, listener);
    }

    public void addWidgetListener(WidgetListener listener) {
        _widgetListeners.add(this, listener);
    }

    public boolean allowsMultipleValues() {
        boolean result;
        if (_associatedCls == null) {
            result = _slot.getAllowsMultipleValues();
        } else {
            result = _associatedCls.getTemplateSlotAllowsMultipleValues(_slot);
        }
        return result;
    }

    public boolean canClose() {
        return true;
    }

    public boolean canSave() {
        return true;
    }

    public void clearSelection() {
        Log.error("Unreachable Code", this, "clearSelection");
    }

    public void close() {
    }

    public boolean configure() {
        WidgetConfigurationPanel panel = createWidgetConfigurationPanel();
        int result = ModalDialog.showDialog(this, panel, "Configure " + getLabel(),
                ModalDialog.MODE_OK_CANCEL, null, false);
        return result == ModalDialog.OPTION_OK;
    }

    protected JSplitPane createLeftRightSplitPane(String locationPropertyName, int defaultLocation) {
        JSplitPane pane = ComponentFactory.createLeftRightSplitPane();
        setSplitPane(pane, locationPropertyName, defaultLocation);
        return pane;
    }

    protected JSplitPane createTopBottomSplitPane(String locationPropertyName, int defaultLocation) {
        JSplitPane pane = ComponentFactory.createTopBottomSplitPane();
        setSplitPane(pane, locationPropertyName, defaultLocation);
        return pane;
    }

    public WidgetConfigurationPanel createWidgetConfigurationPanel() {
        WidgetConfigurationPanel widgetPanel = new WidgetConfigurationPanel((SlotWidget)this);
        if (_buttonInfo != null) {
            ButtonConfigurationPanel buttonPanel = new ButtonConfigurationPanel(getPropertyList());
            widgetPanel.addTab("Buttons", buttonPanel);
            Iterator i = _buttonInfo.iterator();
            while (i.hasNext()) {
                ButtonInfo info = (ButtonInfo) i.next();
                String name = (String) info.action.getValue(Action.NAME);
                String defaultDescription = (String) info.action.getValue(Action.SHORT_DESCRIPTION);
                buttonPanel.addButton(name, defaultDescription, info.defaultState);
            }
        }
        return widgetPanel;
    }

    protected boolean displayButton(String propertyName) {
        Boolean b = getPropertyList().getBoolean(propertyName);
        return (b == null) ? true : b.booleanValue();
    }

    public boolean displayButton(Action action) {
        Boolean b = getPropertyList().getBoolean(getDisplayPropertyName(action));
        return (b == null) ? true : b.booleanValue();
    }

    public void dispose() {
        if (_instance != null) {
            _instance.removeFrameListener(_instanceListener);
        }
    }

    public void finalize() {
        try {
            super.finalize();
            // System.out.println(getClass().getName() + " finalize");
        } catch (Throwable t) {
            t.printStackTrace();
        }
    }

    public Cls getAssociatedCls() {
        return _associatedCls;
    }

    public String getButtonDescription(Action action) {
        return getPropertyList().getString(ButtonConfigurationPanel.getDescriptionPropertyName((String) action.getValue(Action.NAME)));
    }

    public Cls getCls() {
        return _cls;
    }

    public WidgetDescriptor getDescriptor() {
        return _descriptor;
    }

    private String getDisplayPropertyName(Action action) {
        return ButtonConfigurationPanel.getDisplayPropertyName((String) action.getValue(Action.NAME));
    }

    public static Object getFirstItem(Collection c) {
        return CollectionUtilities.getFirstItem(c);
    }

    public Icon getIcon() {
        return _icon;
    }

    public Instance getInstance() {
        return _instance;
    }

    protected String getInvalidValueText(Collection values) {
        String result;
        int count = values.size();
        int min = getMinimumCardinality();
        int max = getMaximumCardinality();
        if (count < min) {
            if (max == 1) {
                result = "Value is required";
            } else {
                result = "At least " + min + " value" + (min == 1 ? " is" : "s are") + " required";
            }
        } else if (max != KnowledgeBase.MAXIMUM_CARDINALITY_UNBOUNDED && count > max) {
            result = "At most " + max + " values are allowed";
        } else {
            result = null;
        }
        return result;
    }

    public KnowledgeBase getKnowledgeBase() {
        return _project.getKnowledgeBase();
    }

    public String getLabel() {
        String label = _descriptor.getLabel();
        if (label == null) {
            // only valid for slot widgets
            Slot slot = getSlot();
            if (slot != null) {
                String text = slot.getBrowserText();
                label = StringUtilities.symbolToLabel(text);
            }
        }
        return label;
    }

    public JMenuBar getMainWindowMenuBar() {
        JMenuBar menuBar;
        JFrame frame = (JFrame) ComponentUtilities.getFrame(this);
        if (frame == null) {
            menuBar = null;
        } else {
            menuBar = frame.getJMenuBar();
        }
        return menuBar;
    }

    private ProjectToolBar getMainWindowToolBar() {
        ProjectToolBar toolBar;
        JRootPane pane = getRootPane();
        if (pane == null) {
            toolBar = null;
        } else {
            toolBar = (ProjectToolBar) pane.getContentPane().getComponent(0);
        }
        return toolBar;
    }

    protected int getMaximumCardinality() {
        return getCls().getTemplateSlotMaximumCardinality(getSlot());
    }

    protected int getMinimumCardinality() {
        return getCls().getTemplateSlotMinimumCardinality(getSlot());
    }

    public Dimension getPreferredSize() {
        Dimension d = super.getPreferredSize();
        if (_preferredColumns > 0) {
            d.width = _preferredColumns * ComponentUtilities.getStandardColumnWidth();
        }
        if (_preferredRows > 0) {
            d.height = _preferredRows * ComponentUtilities.getStandardRowHeight();
        }
        return d;
    }

    public Project getProject() {
        return _project;
    }

    public PropertyList getPropertyList() {
        /*
        if (_propertyList == null) {
            _propertyList = _descriptor.getPropertyList();
        }
        return _propertyList;
        */
        return _descriptor.getPropertyList();
    }

    public Collection getSelection() {
        Log.error("Unreachable Code", this, "getSelection");
        return Collections.EMPTY_LIST;
    }

    public String getShortDescription() {
        return _shortDescription;
    }

    public Slot getSlot() {
        return _slot;
    }

    public String getStringProperty(String name, String defaultString) {
        String property = getPropertyList().getString(name);
        if (property == null) {
            property = defaultString;
        }
        return property;
    }

    public Collection getValues() {
        return Collections.EMPTY_LIST;
    }

    protected void handleBrowserTextChanged() {
        postWidgetEvent(WidgetEvent.LABEL_CHANGED);
    }

    protected void handleFrameNameChanged() {
        postWidgetEvent(WidgetEvent.LABEL_CHANGED);
    }

    protected void handleOwnSlotValueChanged(Slot slot) {
        if (slot == _slot) {
            loadValues();
        }
    }

    public boolean isDesignTime() {
        return _isDesignTime;
    }

    public boolean isRuntime() {
        return !_isDesignTime;
    }

    public boolean isSlotAtCls() {
        return _associatedCls != null;
    }

    public static boolean isSuitable(Cls cls, Slot slot, Facet facet) {
        return false;
    }

    public void loadValues() {
        if (_instance != null && !_isBusy) {
            _isBusy = true;
            try {
                setWidgetValues();
            } finally {
                _isBusy = false;
            }
        }
    }

    public void notifySelectionListeners() {
        _selectionListeners.postEvent(this, SelectionEvent.SELECTION_CHANGED);
    }

    public void postWidgetEvent(int type) {
        // Log.enter(this, "postWidgetEvent", new Integer(type));
        _widgetListeners.postEvent(this, type);
    }

    private void recordDefault(Action action, boolean defaultState) {
        String name = getDisplayPropertyName(action);
        if (getPropertyList().getBoolean(name) == null) {
            getPropertyList().setBoolean(name, defaultState);
        }
    }

    public void removeMainWindowToolBarButton(JButton button) {
        ProjectToolBar toolBar = getMainWindowToolBar();
        if (toolBar != null) {
            toolBar.remove(button);
        }
    }

    public void removeSelectionListener(SelectionListener listener) {
        _selectionListeners.remove(this, listener);
    }

    public void removeWidgetListener(WidgetListener listener) {
        _widgetListeners.remove(this, listener);
    }

    public void reshape(int x, int y, int w, int h) {
        super.reshape(x, y, w, h);
        if (_isDesignTime) {
            // Log.trace("design time", this, "reshape", getBounds());
            _descriptor.setBounds(getBounds());
        }
    }

    public void save() {
        saveSplitterLocations();
    }

    private void saveSplitterLocations() {
        if (_splitPanes != null) {
            Iterator i = _splitPanes.iterator();
            while (i.hasNext()) {
                JSplitPane pane = (JSplitPane) i.next();
                int location = pane.getDividerLocation();
                getPropertyList().setInteger(pane.getName(), location);
            }
        }
    }

    protected static void setAllowed(AllowableAction action, boolean state) {
        if (action != null) {
            action.setAllowed(state);
        }
    }

    public void setAssociatedCls(Cls cls) {
        if (_associatedCls != null) {
            _associatedCls.removeClsListener(_associatedClsListener);
        }
        _associatedCls = cls;
        if (_associatedCls != null) {
            _associatedCls.addClsListener(_associatedClsListener);
        }
        loadValues();
    }

    public void setCls(Cls cls) {
        this._cls = cls;
    }

    public void setEditable(boolean b) {
        // do nothing
    }

    public void setIcon(Icon icon) {
        _icon = icon;
    }

    public void setInstance(Instance newInstance) {
        if (_instance != null) {
            _instance.removeFrameListener(_instanceListener);
        }
        this._instance = newInstance;
        if (_instance != null) {
            _instance.addFrameListener(_instanceListener);
        }
        loadValues();
    }

    public void setInstance(Slot newSlotInstance, Cls newAssociatedCls) {
        if (_instance != null) {
            _instance.removeFrameListener(_instanceListener);
        }
        this._instance = newSlotInstance;
        if (_instance != null) {
            _instance.addFrameListener(_instanceListener);
        }
        _associatedCls = newAssociatedCls;
        loadValues();
        boolean editable = (_instance == null) ? false : _instance.isEditable();
        setEditable(editable);
    }

    public void setInstanceValues() {
        if (_slot != null) {
            Collection values = getValues();
            if (_associatedCls == null) {
                Collection templateSlotValues = _instance.getDirectType().getTemplateSlotValues(_slot);
                if (!templateSlotValues.isEmpty()) {
                    values = new ArrayList(values);
                    values.removeAll(templateSlotValues);
                }
                _instance.setOwnSlotValues(_slot, values);
                updateBorder(values);
            } else {
                Slot instanceSlot = (Slot) _instance;
                _associatedCls.setTemplateFacetValues(instanceSlot, _slot.getAssociatedFacet(), values);
            }
        }
    }

    public void setLabel(String label) {
        _descriptor.setLabel(label);
    }

    public void setPreferredColumns(int nColumns) {
        _preferredColumns = nColumns;
    }

    public void setPreferredRows(int nRows) {
        _preferredRows = nRows;
    }

    public void setPreferredSize(Dimension size) {
        super.setPreferredSize(size);
        _preferredColumns = 0;
        _preferredRows = 0;
    }

    public void setPropertyList(PropertyList list) {
        Assert.assertTrue("design time", isDesignTime());
        _descriptor.setPropertyList(list);
    }

    public void setShortDescription(String description) {
        _shortDescription = description;
    }

    public void setSlot(Slot slot) {
        this._slot = slot;
    }

    private void setSplitPane(JSplitPane pane, String name, int defaultLocation) {
        if (_splitPanes == null) {
            _splitPanes = new ArrayList();
        }
        pane.setName(name);
        _splitPanes.add(pane);
        Integer locationInteger = getPropertyList().getInteger(name);
        int location;
        if (locationInteger == null) {
            location = defaultLocation;
        } else {
            location = locationInteger.intValue();
        }
        pane.setDividerLocation(location);
    }

    public void setup(final WidgetDescriptor descriptor, boolean isDesignTime, Project project, Cls cls, Slot slot) {
        _descriptor = descriptor;
        _isDesignTime = isDesignTime;
        _project = project;
        _cls = cls;
        _slot = slot;

        String text = "???";
        if (slot != null) {
            text = slot.getName();
        } else if (cls != null) {
            text = cls.getName();
        }
        Dimension d = getSize();
        if (d.width > 0 && d.height > 0) {
            setPreferredSize(d);
        }
    }

    public void setValues(Collection values) {
    }

    public void setWidgetValues() {
        if (_slot != null) {
            Collection values;
            if (_associatedCls == null) {
                Collection valuesFromOwn = _instance.getOwnSlotValues(_slot);
                Collection valuesFromTemplate = _instance.getDirectType().getTemplateSlotValues(_slot);
                values = new ArrayList(valuesFromTemplate);
                values.addAll(valuesFromOwn);
                boolean editable = _instance.isEditable();
                setEditable(editable);
            } else {
                Slot instanceSlot = (Slot) _instance;
                Facet facet = _slot.getAssociatedFacet();
                if (facet == null) {
                    values = Collections.EMPTY_LIST;
                    setEditable(false);
                } else {
                    values = _associatedCls.getTemplateFacetValues(instanceSlot, _slot.getAssociatedFacet());
                    boolean editable = _associatedCls.isEditable();
                    setEditable(editable);
                }
            }
            try {
                setValues(values);
                updateBorder(values);
            } catch (Exception e) {
                Log.exception(e, this, "setWidgetValues");
                setValues(Collections.EMPTY_LIST);
            }
        }
    }

    public void show(Cls cls, Slot slot) {
        getProject().show(cls, slot);
    }

    public void showInstance(Instance instance) {
        getProject().show(instance);
    }

    public String toString() {
        return "AbstractWidget(" + _cls + "," + _slot + "," + _instance + ")";
    }

    /** this is a hack because these methods just belong in AbstractSlotWidget */
    protected void updateBorder(Collection c) {
    }

    public void valueChanged() {
        if (_instance != null && !_isBusy) {
            _isBusy = true;
            try {
                setInstanceValues();
            } finally {
                _isBusy = false;
            }
        }
    }
}
