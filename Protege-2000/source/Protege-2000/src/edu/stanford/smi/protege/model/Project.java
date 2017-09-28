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

package edu.stanford.smi.protege.model;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.*;
import javax.swing.*;
import edu.stanford.smi.protege.event.*;
import edu.stanford.smi.protege.resource.*;
import edu.stanford.smi.protege.storage.clips.*;
import edu.stanford.smi.protege.widget.*;
import edu.stanford.smi.protege.ui.*;
import edu.stanford.smi.protege.util.*;

/**
 * The aggregation of a domain knowledge base with its user interface.
 *
 * Methods on this class that take an "errors" collection may insert any object into this collection
 * These objects can be strings or exceptions.  All that is guaranteed is that the toString()
 * method on each object will produce a usable error message.  If the method call succeeds then
 * no error objects will have been added to the collection.  Eventually this hack will be
 * replaced with some more reasonable interface for collecting errors.  Note that we do not want
 * to throw an exception because we would like to accumulate errors (e.g. parse errors) and let
 * the user know about them all at once rather than one at a time.  One downside of the current
 * approach is that it leads to cascading errors.
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class Project {
    private final static String CLASS_PROJECT = "Project";
    private final static String SLOT_DEFAULT_INSTANCE_WIDGET_CLASS_NAME = "default_instance_widget_class_name";
    private final static String SLOT_CUSTOMIZED_INSTANCE_WIDGETS = "customized_instance_widgets";
    private final static String SLOT_BROWSER_SLOTS = "browser_slot_names";
    private final static String SLOT_TABS = "tabs";
    private final static String SLOT_INCLUDED_PROJECTS = "included_projects";
    private final static String SLOT_ALL_KNOWLEDGE_BASE_FACTORY_NAMES = "all_knowledge_base_factory_names";
    private final static String SLOT_SOURCES = "sources";
    private final static String SLOT_JAVA_PACKAGES = "java_packages";
    private final static String SLOT_HIDDEN_CLASSES = "hidden_classes";
    private final static String SLOT_JOURNALING_ENABLED = "journaling_enabled";
    private final static String SLOT_DEFAULT_CLS_METACLASS = "default_cls_metaclass";
    private final static String SLOT_DEFAULT_SLOT_METACLASS = "default_slot_metaclass";
    private final static String SLOT_DEFAULT_FACET_METACLASS = "default_facet_metaclass";
    private final static String SLOT_NEXT_FRAME_NUMBER = "next_frame_number";
    private final static String SLOT_IS_READONLY = "is_readonly";

    private final static String CLASS_OPTIONS = "Options";
    private final static String SLOT_OPTIONS = "options";
    private final static String SLOT_DISPLAY_HIDDEN_CLASSES = "display_hidden_classes";
    private final static String SLOT_DISPLAY_ABSTRACT_CLASS_ICON = "display_abstract_class_icon";
    private final static String SLOT_DISPLAY_MULTI_PARENT_CLASS_ICON = "display_multi_parent_class_icon";
    private final static String SLOT_DISPLAY_REMOVE_CONFIRMATION_DIALOG = "confirm_on_remove";
    private final static String SLOT_UPDATE_MODIFICATION_SLOTS = "update_modification_slots";

    private final static String CLASS_MAP = "Map";
    private final static String SLOT_PROPERTY_MAP = "property_map";

    private String _filePath;
    private KnowledgeBase _projectKB;
    private Instance _projectInstance;
    private KnowledgeBase _domainKB;
    private String _defaultInstanceWidgetClassName;
    private Map _activeClsWidgetDescriptors = new HashMap(); // <Cls, WidgetDescriptor>
    private Map _cachedDesignTimeClsWidgets = new HashMap(); 		// Cls -> ClsWidget
    private Map _frames = new HashMap(); 	// <Instance or FrameSlotPair, JFrame>
    private Map _objects = new HashMap(); 	// <JFrame, Instance or FrameSlotPair>
    private WidgetMapper _widgetMapper;
    private Collection _cachedIncludedProjectFiles = new ArrayList();
    private Point _lastLocation;
    private ListenerCollection _listeners = new ListenerList(new ProjectEventDispatcher());
    private Boolean _displayHiddenClasses;
    private Boolean _displayAbstractClassIcon;
    private Boolean _displayMultiParentClassIcon;
    private Boolean _displayConfirmationOnRemove;
    private Boolean _isReadonly;
    private Boolean _updateModificationSlots;
    private Map _includedBrowserSlots = new HashMap();	// <Cls, Slot>
    private Map _directBrowserSlots = new HashMap();	// <Cls, Slot>
    private Set _hiddenFrames = new HashSet();			// <Cls>
    private Set _includedHiddenFrames = new HashSet();  // <Cls>
    private boolean _hasChanged;
    private Collection _tabWidgetDescriptors;

    private WindowListener _closeListener = new WindowAdapter() {
        public void windowClosing(WindowEvent event) {
            JFrame frame = (JFrame) event.getWindow();
            frame.setVisible(false);
            Object o = _objects.remove(frame);
            Assert.assertNotNull("frame", o);
            _frames.remove(o);
            _objects.remove(frame);
            ComponentUtilities.dispose(frame);
            edu.stanford.smi.protege.Application.repaint();
        }
    };

    private KnowledgeBaseListener _knowledgeBaseListener = new KnowledgeBaseAdapter() {
        public void clsCreated(KnowledgeBaseEvent event) {
            // do nothing
        }

        public void clsDeleted(KnowledgeBaseEvent event) {
            Cls cls = event.getCls();
            _activeClsWidgetDescriptors.remove(cls);
            ClsWidget widget = (ClsWidget) _cachedDesignTimeClsWidgets.remove(cls);
            if (widget != null) {
                ComponentUtilities.dispose((Component) widget);
            }
            _directBrowserSlots.remove(cls);
            removeDisplay(cls);
            _hiddenFrames.remove(cls);
        }

        public void frameNameChanged(KnowledgeBaseEvent event) {
            Frame frame = event.getFrame();
            WidgetDescriptor d = (WidgetDescriptor) _activeClsWidgetDescriptors.get(frame);
            if (d != null) {
                d.setName(frame.getName());
            }
        }

        public void facetDeleted(KnowledgeBaseEvent event) {
            removeDisplay(event.getFrame());
        }

        public void slotDeleted(KnowledgeBaseEvent event) {
            Slot slot = (Slot) event.getFrame();
            removeDisplay(slot);
            Iterator i = _directBrowserSlots.entrySet().iterator();
            while (i.hasNext()) {
                Map.Entry entry = (Map.Entry) i.next();
                if (entry.getValue() == slot) {
                    i.remove();
                }
            }
        }

        public void instanceDeleted(KnowledgeBaseEvent event) {
            super.instanceDeleted(event);
            // Log.enter(this, "instanceDeleted");
            removeDisplay(event.getFrame());
        }
    };

    private Map _propertyMap = new HashMap();

    static {
        SystemUtilities.init();
    }

    private Project(
        String fileName,
        KnowledgeBaseFactory factory,
        Collection errors,
        boolean createDomainKB) {
        setProjectFilePath(fileName);
        _projectKB = loadProjectKB(fileName, factory, errors);
        if (_projectKB != null) {
            _projectInstance = getProjectInstance(_projectKB);
        }
        if (_projectInstance != null && createDomainKB) {
            boolean load = fileName != null;
            createDomainKnowledgeBase(factory, errors, load);
        }
        if (_projectKB != null) {
            setupJournaling();
        }
    }

    /**
     * creates a project and loads the project kb from the project file and the associated domain kb
     *
     * @param errors See class note for information about this argument.
     */
    public Project(String filename, Collection errors) {
        this(filename, null, errors, true);
    }

    public void addJavaPackageName(String packageName) {
        addProjectSlotValue(SLOT_JAVA_PACKAGES, packageName);
        _domainKB.addJavaLoadPackage(packageName);
    }

    public void addProjectListener(ProjectListener listener) {
        _listeners.add(this, listener);
    }

    private void addProjectSlotValue(String slotName, Object value) {
        ModelUtilities.addOwnSlotValue(_projectInstance, slotName, value);
    }

    private void clearProjectFrameMaps() {
    }

    /**
     * @param errors See class note for information about this argument.
     */
    public static Project createBuildProject(KnowledgeBaseFactory factory, Collection errors) {
        return new Project(null, factory, errors, false);
    }

    private void createDomainKB(KnowledgeBaseFactory factory, Collection errors) {
        if (factory == null) {
            factory = getKnowledgeBaseFactory();
        }
        _domainKB = factory.createKnowledgeBase(errors);
        Iterator i = getProjectSlotValues(SLOT_JAVA_PACKAGES).iterator();
        while (i.hasNext()) {
            String name = (String) i.next();
            _domainKB.addJavaLoadPackage(name);
        }
        _domainKB.setProject(this);
        _domainKB.setName(getName());
        setKnowledgeBaseFactory(factory);
    }

    /**
     * @param errors See class note for information about this argument.
     */
    public void createDomainKnowledgeBase(KnowledgeBaseFactory factory, Collection errors, boolean load) {
        createDomainKB(factory, errors);
        if (load) {
            loadIncludedProjects(_projectInstance, errors);
            loadDomainKB(errors);
        }
        _domainKB.addKnowledgeBaseListener(_knowledgeBaseListener);
        loadCachedKnowledgeBaseObjects(_projectInstance);
        _domainKB.setEventsEnabled(true);
        _domainKB.setChanged(false);
        _projectKB.setChanged(false);
    }

    /**
     * @param errors See class note for information about this argument.
     */
    public static Project createFileProject(String fileName, Collection errors) {
        return new Project(fileName, errors);
    }


    private JFrame createFrame(Cls cls, Slot slot) {
        InstanceDisplay d = new InstanceDisplay(this, false, false);
        d.setInstance(slot);
        d.getCurrentClsWidget().setAssociatedCls(cls);
        return createFrame(d, new FrameSlotCombination(cls, slot));
    }


    private JFrame createFrame(Instance instance) {
        InstanceDisplay d = new InstanceDisplay(this, true, false);
        d.setInstance(instance);
        return createFrame(d, instance);
    }

    private JFrame createFrame(InstanceDisplay display, Object o) {
        final JFrame frame = ComponentFactory.createFrame();
        frame.addWindowListener(_closeListener);
        frame.getContentPane().add(display, BorderLayout.CENTER);
        ComponentUtilities.pack(frame);
        ClsWidget widget = display.getCurrentClsWidget();
        frame.setTitle(widget.getLabel());
        widget.addWidgetListener(
            new WidgetAdapter() {
                public void labelChanged(WidgetEvent event) {
                    frame.setTitle(event.getWidget().getLabel());
                }
            }
        );
        setLocation(frame);
        frame.show();
        _frames.put(o, frame);
        _objects.put(frame, o);
        return frame;
    }

    /**
     * @param errors See class note for information about this argument.
     */
    public static Project createNewProject(KnowledgeBaseFactory factory, Collection errors) {
        return new Project(null, factory, errors, true);
    }

    private void createNewTabWidgetDescriptors(Collection names) {
        Iterator i = names.iterator();
        while (i.hasNext()) {
            String name = (String) i.next();
            WidgetDescriptor d = WidgetDescriptor.create(_projectKB);
            d.setWidgetClassName(name);
            d.setVisible(false);
            _tabWidgetDescriptors.add(d);
        }
    }

    public ClsWidget createRuntimeClsWidget(Instance instance) {
        ClsWidget widget;
        Cls cls = instance.getDirectType();
        if (cls == null) {
            Log.error("no direct type", this, "createRuntimeClsWidget", instance);
            widget = new UglyClsWidget();
        } else {
            ClsWidget designTimeWidget = getDesignTimeClsWidget(cls);
            WidgetDescriptor d = designTimeWidget.getDescriptor();
            widget = WidgetUtilities.createClsWidget(d, false, this, cls);
        }
        widget.setInstance(instance);
        return widget;
    }

    /**
     * @deprecated use createRuntimeClsWidget
     */
        public Widget createRuntimeWidget(Instance instance) {
            return createRuntimeClsWidget(instance);
        }

    public WidgetDescriptor createWidgetDescriptor() {
        return WidgetDescriptor.create(_projectKB);
    }

    public WidgetDescriptor createWidgetDescriptor(Cls cls, Slot slot, Facet facet) {
        return _widgetMapper.createWidgetDescriptor(cls, slot, facet);
    }

    public void dispose() {
        postProjectEvent(ProjectEvent.PROJECT_CLOSED);
        _domainKB.dispose();
        _projectKB.dispose();
        _domainKB = null;
        _projectKB = null;
    }

    public static boolean equals(Object o1, Object o2) {
        return (o1 == null) ? (o2 == null) : o1.equals(o2);
    }

    public void finalize() throws Throwable {
        if (_domainKB != null) {
            dispose();
        }
        super.finalize();
    }

    private void flushProjectKBCache() {
        saveBrowserSlots();
        saveCustomizedWidgets();
        saveDefaultMetaclasses();
        saveHiddenClassFlags();
        saveNextFrameNumber();
        savePropertyMap();
    }

    public Collection getAllKnowledgeBaseFactories() {
        Collection factories = new ArrayList();
        Iterator i = getProjectSlotValues(SLOT_ALL_KNOWLEDGE_BASE_FACTORY_NAMES).iterator();
        while (i.hasNext()) {
            String name = (String) i.next();
            factories.add(SystemUtilities.newInstance(name));
        }
        return factories;
    }

    public Slot getBrowserSlot(Cls cls) {
        Slot slot = getDirectBrowserSlot(cls);
        if (slot == null) {
            Iterator i = cls.getDirectSuperclasses().iterator();
            while (i.hasNext() && slot == null) {
                Cls superclass = (Cls) i.next();
                slot = getBrowserSlot(superclass);
            }
        }
        if (slot == null) {
            slot = cls.getKnowledgeBase().getSlot(Model.Slot.NAME);
        }
        return slot;
    }

    public PropertyList getClsWidgetPropertyList(Cls cls) {
        ClsWidget widget = getDesignTimeClsWidget(cls);
        PropertyList list = widget.getDescriptor().getPropertyList();
        return list;
    }

    public String getDefaultWidgetClassName(Cls cls, Slot slot, Facet facet) {
        return _widgetMapper.getDefaultWidgetClassName(cls, slot, facet);
    }

    public ClsWidget getDesignTimeClsWidget(Cls cls) {
        ClsWidget widget = (ClsWidget) _cachedDesignTimeClsWidgets.get(cls);
        if (widget == null) {
            // Log.enter(this, "createClsWidget", cls, new Boolean(designTime));
            WidgetDescriptor d = getClsWidgetDescriptor(cls);
            widget = WidgetUtilities.createClsWidget(d, true, this, cls);
            _cachedDesignTimeClsWidgets.put(cls, widget);
        } else if (!widget.getDescriptor().isDirectlyCustomizedByUser()) {
            widget.removeCustomizations();
        }
        return widget;
    }

    public Slot getDirectBrowserSlot(Cls cls) {
        return (Slot) _directBrowserSlots.get(cls);
    }

    public boolean getDisplayAbstractClassIcon() {
        if (_displayAbstractClassIcon == null) {
            _displayAbstractClassIcon = loadOption(SLOT_DISPLAY_ABSTRACT_CLASS_ICON, true);
        }
        return _displayAbstractClassIcon.booleanValue();
    }

    public boolean getDisplayConfirmationOnRemove() {
        if (_displayConfirmationOnRemove == null) {
            _displayConfirmationOnRemove = loadOption(SLOT_DISPLAY_REMOVE_CONFIRMATION_DIALOG, false);
        }
        return _displayConfirmationOnRemove.booleanValue();
    }

    public boolean getDisplayHiddenClasses() {
        if (_displayHiddenClasses == null) {
            _displayHiddenClasses = loadOption(SLOT_DISPLAY_HIDDEN_CLASSES, true);
        }
        return _displayHiddenClasses.booleanValue();
    }

    public boolean getDisplayMultiParentClassIcon() {
        if (_displayMultiParentClassIcon == null) {
            _displayMultiParentClassIcon = loadOption(SLOT_DISPLAY_MULTI_PARENT_CLASS_ICON, true);
        }
        return _displayMultiParentClassIcon.booleanValue();
    }

    private Image getImage(Instance instance) {
        Image image;
        if (instance instanceof Cls) {
            image = Icons.getClsImage();
        } else if (instance instanceof Slot) {
            image = Icons.getSlotImage();
        } else if (instance instanceof Facet) {
            image = Icons.getFacetImage();
        } else {
            image = Icons.getInstanceImage();
        }
        return image;
    }

    /**
     * @return the names of all included projects, including indirectly included ones
     */
    public Collection getIncludedProjects() {
        return Collections.unmodifiableCollection(_cachedIncludedProjectFiles);
    }

    /**
     * @return the names of all directly included projects
     */
    public Collection getDirectIncludedProjects() {
        return getProjectSlotValues(SLOT_INCLUDED_PROJECTS);
    }

    public KnowledgeBase getInternalProjectKnowledgeBase() {
        return _projectKB;
    }

    public String getJournalFile() {
        String journalFile = null;
        if (_filePath != null) {
            journalFile = FileUtilities.getBaseName(_filePath) + ".pjrn";
        }
        return journalFile;
    }

    public KnowledgeBase getKnowledgeBase() {
        return _domainKB;
    }

    public KnowledgeBaseFactory getKnowledgeBaseFactory() {
        KnowledgeBaseFactory factory;
        String name = getSources().getString(KnowledgeBaseFactory.FACTORY_CLASS_NAME);
        if (name == null) {
            factory = new ClipsKnowledgeBaseFactory();
        } else {
            factory = (KnowledgeBaseFactory) SystemUtilities.newInstance(name);
        }
        return factory;
    }

    public String getName() {
        return (_filePath == null) ? "KB" : FileUtilities.getBaseName(_filePath);
    }

    private String getNamespaceName() {
        String s = null;
        if (_filePath != null) {
            s = new File(_filePath).getName();
            int index = s.indexOf('.');
            s = s.substring(0, index);
        }
        return s;
    }

    public Collection getOpenWindows() {
        return Collections.unmodifiableCollection(_frames.values());
    }

    private boolean getOption(String slotName, boolean defaultValue) {
        Boolean b = (Boolean) getOwnSlotValue(getOptionsInstance(), slotName);
        return (b == null) ? defaultValue : b.booleanValue();
    }

    private Instance getOptionsInstance() {
        Instance instance = (Instance) getProjectSlotValue(SLOT_OPTIONS);
        if (instance == null) {
            Cls optionsCls = _projectKB.getCls(CLASS_OPTIONS);
            instance = _projectKB.createInstance(null, optionsCls);
            setProjectSlotValue(SLOT_OPTIONS, instance);
        }
        return instance;
    }

    private static String getOwnSlotStringValue(Frame frame, String slotName) {
        return (String) getOwnSlotValue(frame, slotName);
    }

    private static Object getOwnSlotValue(Frame frame, String slotName) {
        return ModelUtilities.getOwnSlotValue(frame, slotName);
    }

    private Reader getProjectClsesReader() {
        Reader reader = Files.getSystemClsesReader();
        if (reader == null) {
            Log.error("Unable to read system ontology", this, "getProjectClsesReader");
        }
        return reader;
    }

    public File getProjectDirectory() {
        return new File(_filePath).getParentFile();
    }

    public File getProjectFile() {
        return new File(_filePath);
    }

    public String getProjectFilePath() {
        return _filePath;
    }

    private static Instance getProjectInstance(KnowledgeBase kb) {
        Instance result = null;
        Cls cls = kb.getCls(CLASS_PROJECT);
        if (cls == null) {
            Log.error("no project class", Project.class, "getProjectInstance", kb);
        } else {
            Collection instances = cls.getDirectInstances();
            // Assert.areEqual(instances.size(), 1);
            return (Instance) CollectionUtilities.getFirstItem(instances);
        }
        return result;
    }

    private Reader getProjectInstancesReader(String filePath, KnowledgeBaseFactory factory, Collection errors) {
        Reader reader = null;
        if (filePath != null) {
            reader = FileUtilities.getReader(filePath);
            if (reader == null) {
                errors.add("Unable to load project file: " + filePath);
            }
        }
        if (reader == null && factory != null) {
            String path = factory.getProjectFilePath();
            if (path != null) {
                reader = FileUtilities.getResourceReader(factory.getClass(), path);
                if (reader == null) {
                    Log.error("Unable to read factory project: " + path, this, "getProjectInstancesReader");
                }
            }
        }
        if (reader == null) {
            reader = Files.getSystemInstancesReader();
            if (reader == null) {
                Log.error("Unable to read system instances", this, "getProjectInstancesReader");
            }
        }
        return reader;
    }

    private Writer getProjectInstancesWriter(String filePath, Collection errors) {
        return FileUtilities.getWriter(filePath);
    }

    public String getProjectName() {
        return FileUtilities.getBaseName(_filePath);
    }

    private static Object getProjectSlotValue(Instance projectInstance, String slotName) {
        return ModelUtilities.getOwnSlotValue(projectInstance, slotName);
    }

    private Object getProjectSlotValue(String slotName) {
        return getProjectSlotValue(_projectInstance, slotName);
    }

    private static Collection getProjectSlotValues(Instance projectInstance, String slotName) {
        return ModelUtilities.getOwnSlotValues(projectInstance, slotName);
    }

    private Collection getProjectSlotValues(String slotName) {
        return getProjectSlotValues(_projectInstance, slotName);
    }

    private static PropertyList getPropertyList(Instance instance, String slotName) {
        PropertyList propertyList;
        Instance plInstance = (Instance) getOwnSlotValue(instance, slotName);
        if (plInstance == null) {
            // Log.trace("creating property list", Project.class, "getPropertyList", instance, slotName);
            propertyList = PropertyList.create(instance.getKnowledgeBase());
            setOwnSlotValue(instance, slotName, propertyList.getWrappedInstance());
        } else {
            propertyList = new PropertyList(plInstance);
        }
        return propertyList;
    }

    private PropertyList getPropertyList(String name) {
        return getPropertyList(_projectInstance, name);
    }

    public Map getPropertyMap() {
        return _propertyMap;
    }

    public PropertyList getSources() {
        return new PropertyList((Instance) getProjectSlotValue(SLOT_SOURCES));
    }

    private static PropertyList getSources(Instance projectInstance) {
        return new PropertyList((Instance) getProjectSlotValue(projectInstance, SLOT_SOURCES));
    }

    public Collection getSuitableWidgetClassNames(Cls cls, Slot slot, Facet facet) {
        return _widgetMapper.getSuitableWidgetClassNames(cls, slot, facet);
    }

    public WidgetDescriptor getTabWidgetDescriptor(String widgetName) {
        WidgetDescriptor descriptor = null;
        Iterator i = getProjectSlotValues(SLOT_TABS).iterator();
        while (i.hasNext()) {
            Instance instance = (Instance) i.next();
            WidgetDescriptor d = WidgetDescriptor.create(instance);
            if (widgetName.equals(d.getWidgetClassName())) {
                descriptor = d;
                break;
            }
        }
        return descriptor;
    }

    public Collection getTabWidgetDescriptors() {
        if (_tabWidgetDescriptors == null) {
            Set availableTabNames = new HashSet(SystemUtilities.getAvailableTabWidgetNames());

            _tabWidgetDescriptors = new ArrayList();
            Iterator i = getProjectSlotValues(SLOT_TABS).iterator();
            while (i.hasNext()) {
                Instance instance = (Instance) i.next();
                WidgetDescriptor d = WidgetDescriptor.create(instance);
                if (SystemUtilities.isLoadableClass(d.getWidgetClassName())) {
                    _tabWidgetDescriptors.add(d);
                } else {
                    Log.warning("removing reference to missing tab: " + d.getWidgetClassName(), this, "getTabWidgetDescriptors");
                }
                String name = d.getWidgetClassName();
                boolean removed = availableTabNames.remove(name);
                if (!removed) {
                    // Log.warning("tab " + name + " not in manifest", this, "getTabWidgetDescriptors");
                }
            }

            createNewTabWidgetDescriptors(availableTabNames);
            saveTabWidgetInstances();
        }
        return _tabWidgetDescriptors;
    }

    public boolean getUpdateModificationSlots() {
        if (_updateModificationSlots == null) {
            _updateModificationSlots = loadOption(SLOT_UPDATE_MODIFICATION_SLOTS, false);
        }
        return _updateModificationSlots.booleanValue();
    }

    private boolean hasChanged() {
        return _hasChanged;
    }

    public boolean hasCompleteSources() {
        Collection errors = new ArrayList();
        KnowledgeBaseFactory factory = getKnowledgeBaseFactory();
        boolean hasCompleteSources = (_filePath != null) && (factory != null);
        if (hasCompleteSources) {
            hasCompleteSources = factory.isComplete(getSources());
        }
        return hasCompleteSources;
    }

    public boolean hasCustomizedDescriptor(Cls cls) {
        WidgetDescriptor d = (WidgetDescriptor) _activeClsWidgetDescriptors.get(cls);
        if (d != null && !d.isDirectlyCustomizedByUser()) {
            d = null;
        }
        return d != null;
    }

    public boolean hasIncludedProjects() {
        return !getIncludedProjects().isEmpty();
    }

    private void includeDomainKB(Instance projectInstance, Collection errors) {
        String name = getSources(projectInstance).getString(KnowledgeBaseFactory.FACTORY_CLASS_NAME);
        KnowledgeBaseFactory factory = (KnowledgeBaseFactory) SystemUtilities.newInstance(name);
        PropertyList sources = getSources(projectInstance);
        factory.includeKnowledgeBase(_domainKB, sources, errors);
    }

    /**
     * @param errors See class note for information about this argument.
     */
    public void includeProject(String fileName, Collection errors) {
        includeProject(fileName, true, errors);
    }

    /**
     * @param errors See class note for information about this argument.
     */
    public void includeProject(String filePath, boolean doLoad, Collection errors) {
        Journal.enter(this, "include project", "file", filePath);
        if (doLoad) {
            loadIncludedProject(filePath, errors);
        }
        // Hack
        if (_filePath == null) {
            FileUtilities.setCurrentWorkingDirectoryFromFile(filePath);
        }
        recordIncludedProject(filePath);
    }

    public boolean isDirty() {
        return _domainKB.hasChanged() || _projectKB.hasChanged() || hasChanged();
    }

    public boolean isHidden(Frame frame) {
        return _hiddenFrames.contains(frame);
    }

    private boolean isIncluded(String filePath) {
        String fullPath = FileUtilities.getAbsolutePath(filePath);
        return _cachedIncludedProjectFiles.contains(fullPath);
    }

    private boolean isIncludedBrowserSlot(Cls cls, Slot slot) {
        return _includedBrowserSlots.get(cls) == slot;
    }

    public boolean isJournalingEnabled() {
        Boolean b = (Boolean) getProjectSlotValue(SLOT_JOURNALING_ENABLED);
        return (b == null) ? false : b.booleanValue();
    }

    public boolean isReadonly() {
        if (_isReadonly == null) {
            _isReadonly = loadOption(SLOT_IS_READONLY, false);

        }
        return _isReadonly.booleanValue();
    }

    public boolean isSuitableWidget(Cls cls, Slot slot, Facet facet, WidgetDescriptor d) {
        return _widgetMapper.isSuitableWidget(cls, slot, facet, d);
    }

    public void loadBrowserSlots(Instance projectInstance) {
        PropertyList browserSlots = getPropertyList(projectInstance, SLOT_BROWSER_SLOTS);
        Iterator i = browserSlots.getNames().iterator();
        while (i.hasNext()) {
            String clsName = (String) i.next();
            if (clsName == null) {
                Log.trace("null class name", this, "loadBrowserSlots");
            } else {
                String slotName = browserSlots.getString(clsName);
                Cls cls = _domainKB.getCls(clsName);
                Slot slot = _domainKB.getSlot(slotName);
                if (cls != null && slot != null) {
                    recordDirectBrowserSlot(cls, slot);
                    if (projectInstance != _projectInstance) {
                        _includedBrowserSlots.put(cls, slot);
                    }
                } else {
                    // Log.warning("Bad frame properties: " + clsName + " " + slotName, this, "loadFrameProperties");
                    browserSlots.remove(clsName);
                }
            }
        }
    }

    private void loadCachedKnowledgeBaseObjects(Instance projectInstance) {
        loadPropertyMap();
        loadNextFrameNumber(projectInstance);
        loadWidgetMapper(projectInstance);
        loadWidgetDescriptors(projectInstance);
        loadBrowserSlots(projectInstance);
        loadDefaultMetaclasses(projectInstance);
        loadHiddenClassFlags(projectInstance);
        _defaultInstanceWidgetClassName =
            (String) getProjectSlotValue(projectInstance, SLOT_DEFAULT_INSTANCE_WIDGET_CLASS_NAME);
    }

    private void loadDefaultMetaclasses(Instance projectInstance) {
        String clsClsName = (String) getProjectSlotValue(projectInstance, SLOT_DEFAULT_CLS_METACLASS);
        if (clsClsName != null && !clsClsName.equals(Model.Cls.STANDARD_CLASS)) {
            Cls clsMetaCls = _domainKB.getCls(clsClsName);
            if (clsMetaCls != null) {
                _domainKB.setDefaultClsMetaCls(clsMetaCls);
            }
        }
        String slotClsName = (String) getProjectSlotValue(projectInstance, SLOT_DEFAULT_SLOT_METACLASS);
        if (slotClsName != null && !slotClsName.equals(Model.Cls.STANDARD_SLOT)) {
            Cls slotMetaCls = _domainKB.getCls(slotClsName);
            if (slotMetaCls != null) {
                _domainKB.setDefaultSlotMetaCls(slotMetaCls);
            }
        }
        String facetClsName = (String) getProjectSlotValue(projectInstance, SLOT_DEFAULT_FACET_METACLASS);
        if (facetClsName != null && !facetClsName.equals(Model.Cls.STANDARD_FACET)) {
            Cls facetMetaCls = _domainKB.getCls(facetClsName);
            if (facetMetaCls != null) {
                _domainKB.setDefaultFacetMetaCls(facetMetaCls);
            }
        }
    }

    private void loadDomainKB(Collection errors) {
        KnowledgeBaseFactory factory = getKnowledgeBaseFactory();
        if (factory != null) {
            _domainKB.setLoading(true);
            factory.loadKnowledgeBase(_domainKB, getSources(), errors);
            _domainKB.setLoading(false);
        }
    }

    private void loadHiddenClassFlags(Instance projectInstance) {
        Iterator i = getProjectSlotValues(projectInstance, SLOT_HIDDEN_CLASSES).iterator();
        while (i.hasNext()) {
            String name = (String) i.next();
            Cls cls = _domainKB.getCls(name);
            if (cls == null) {
                Log.trace("class not found: " + name, this, "loadHiddenClassFlags");
            } else {
                recordHidden(cls, true);
                if (isIncluded(projectInstance)) {
                    _includedHiddenFrames.add(cls);
                }
            }
        }
    }

    private boolean isIncluded(Instance projectInstance) {
        return _projectInstance != projectInstance;
    }

    private void loadIncludedProject(String filePath, Collection errors) {
        if (!isIncluded(filePath)) {
            // Log.enter(this, "loadIncludedProject", filePath);
            FileUtilities.pushCurrentWorkingDirectoryFromFile(filePath);
            KnowledgeBase kb = loadProjectKB(filePath, null, errors);
            if (kb != null && errors.size() == 0) {
                kb.setName(FileUtilities.getBaseName(filePath));
                Instance projectInstance = getProjectInstance(kb);
                loadIncludedProjects(projectInstance, errors);
                includeDomainKB(projectInstance, errors);
                loadCachedKnowledgeBaseObjects(projectInstance);
                _cachedIncludedProjectFiles.add(FileUtilities.getAbsolutePath(filePath));
                FileUtilities.popCurrentWorkingDirectory();
            }
        }
    }

    /**
     * @param errors See class note for information about this argument.
     */
    public void loadIncludedProjects(Instance projectInstance, Collection errors) {
        Iterator i = getProjectSlotValues(projectInstance, SLOT_INCLUDED_PROJECTS).iterator();
        while (i.hasNext()) {
            String name = (String) i.next();
            loadIncludedProject(name, errors);
        }
    }

    private void loadNextFrameNumber(Instance projectInstance) {
        Integer i = (Integer) getProjectSlotValue(SLOT_NEXT_FRAME_NUMBER);
        int number;
        if (i == null) {
            number = ApplicationProperties.getOldNextFrameNumber();
        } else {
            number = i.intValue();
        }

        int nextFrameNumber = Math.max(_domainKB.getNextFrameNumber(), number);
        ;
        _domainKB.setNextFrameNumber(nextFrameNumber);
    }

    private Boolean loadOption(String name, boolean defaultValue) {
        boolean b = getOption(name, defaultValue);
        return new Boolean(b);
    }

    /**
     * @param errors See class note for information about this argument.
     */
    public static Project loadProjectFromFile(String fileName, Collection errors) {
        return new Project(fileName, null, errors, true);
    }

    private KnowledgeBase loadProjectKB(String filePath,
            KnowledgeBaseFactory factory, Collection errors) {
        KnowledgeBase kb = null;
        Reader clsesReader = null;
        Reader instancesReader = null;
        try {
            clsesReader = getProjectClsesReader();
            instancesReader = getProjectInstancesReader(filePath, factory, errors);
            if (instancesReader == null) {
                errors.add("Unable to open project file: " + filePath);
            } else {
                kb = new ClipsKnowledgeBaseFactory().loadKnowledgeBase(clsesReader, instancesReader, errors);
                String name = "";
                if (filePath != null && instancesReader != null) {
                    name = FileUtilities.getBaseName(filePath) + "_";
                }
                kb.setName(name + "ProjectKB");
                if (errors.size() == 0) {
                    BackwardsCompatibilityProjectFixups.fix(kb);
                }
                // kb.setEventsEnabled(true);

                // This should really be done on save but it is difficult then because
                // there are lot of unreferenced instances that have to be kept in memory.
                // It is safe to do it here.  The downside is that the unreferenced
                // instances are actually written to file.
                removeUnreferencedInstances(kb);
            }
        } catch (Exception e) {
            errors.add(e);
        } finally {
            SystemUtilities.close(clsesReader);
            SystemUtilities.close(instancesReader);
        }
        return kb;
    }

    private void loadPropertyMap() {
        Instance instance = (Instance) getOwnSlotValue(_projectInstance, SLOT_PROPERTY_MAP);
        if (instance == null) {
            _propertyMap = new HashMap();
        } else {
            _propertyMap = PropertyMapUtil.load(instance, _domainKB);
        }
    }

    private void loadWidgetDescriptors(Instance projectInstance) {
        Iterator i = new ArrayList(getProjectSlotValues(projectInstance, SLOT_CUSTOMIZED_INSTANCE_WIDGETS)).iterator();
        while (i.hasNext()) {
            Instance instance = (Instance) i.next();

            // duplicate included widget descriptors into main project
            if (projectInstance != _projectInstance) {
                instance = (Instance) instance.deepCopy(_projectKB, null);
            }

            WidgetDescriptor d = WidgetDescriptor.create(instance);
            if (d == null) {
                Log.error("Invalid widget instance: " + instance, this, "loadWidgetDescriptors");
                removeProjectSlotValue(SLOT_CUSTOMIZED_INSTANCE_WIDGETS, instance);
            } else {
                Cls cls = _domainKB.getCls(d.getName());
                if (cls == null) {
                    Log.warning("unknown class " + d.getName(), this, "loadWidgetDescriptors");
                    removeProjectSlotValue(SLOT_CUSTOMIZED_INSTANCE_WIDGETS, instance);
                } else {
                    // Log.trace("Loading widget for class " + d.getName(), this, "loadWidgetDescriptors");
                    _activeClsWidgetDescriptors.put(cls, d);
                    d.setDirectlyCustomizedByUser(true);
                    if (projectInstance == _projectInstance) {
                        // d.setCustomized(true);
                        // Log.trace("set modified", this, "loadWidgetDescriptors", projectInstance);
                    } else {
                        d.setIncluded(true);
                        // Log.trace("set included", this, "loadWidgetDescriptors", projectInstance);
                    }
                }
            }
        }
    }

    private void loadWidgetMapper(Instance projectInstance) {
        _widgetMapper = new DefaultWidgetMapper(_projectKB, this);
    }

    private void makeTemporaryWidgetsIncluded(boolean b) {
        Iterator i = _activeClsWidgetDescriptors.values().iterator();
        while (i.hasNext()) {
            WidgetDescriptor d = (WidgetDescriptor) i.next();
            if (d.isTemporary()) {
                d.setIncluded(b);
            }
        }
    }

    public void mergeIncludedFrames() {
        Iterator i = _domainKB.getFrames().iterator();
        while (i.hasNext()) {
            Frame frame = (Frame) i.next();
            if (!frame.isSystem()) {
                frame.setIncluded(false);
            }
        }
    }

    public void mergeIncludedProjects() {
        mergeIncludedFrames();
        removeIncludedProjectReferences();
    }

    public void postFormChangeEvent(Cls cls) {
        ClsWidget widget = (ClsWidget) _cachedDesignTimeClsWidgets.remove(cls);
        if (widget != null) {
            widget.dispose();
        }
        postProjectEvent(ProjectEvent.FORM_CHANGED, cls);
    }

    public void postFormChangeEvent(WidgetDescriptor d) {
        Cls cls = getKnowledgeBase().getCls(d.getName());
        postFormChangeEvent(cls);
    }

    public void postProjectEvent(int type) {
        postProjectEvent(type, null);
    }

    public void postProjectEvent(int type, Object arg1) {
        _listeners.postEvent(this, type, arg1);
    }

    private void recordDirectBrowserSlot(Cls cls, Slot slot) {
        _directBrowserSlots.put(cls, slot);
    }

    private void recordHidden(Frame frame, boolean hidden) {
        if (hidden) {
            _hiddenFrames.add(frame);
        } else {
            if (_includedHiddenFrames.contains(frame)) {
                Log.trace("Cannot 'unhide' an included hidden frame", this, "recordHidden");
            } else {
                _hiddenFrames.remove(frame);
            }
        }
    }

    private void recordIncludedProject(String name) {
        String relativeName = FileUtilities.relativePath(name);
        Log.trace("relativeName=" + relativeName, this, "recordIncludedProject", name);
        addProjectSlotValue(SLOT_INCLUDED_PROJECTS, relativeName);
    }

    private void removeDisplay(Frame frame) {
        JFrame jframe = (JFrame) _frames.get(frame);
        if (jframe != null) {
            ComponentUtilities.closeWindow(jframe);
        }
    }

    public void removeIncludedProjectReferences() {
        Map browserSlots = new HashMap();
        browserSlots.putAll(_includedBrowserSlots);
        browserSlots.putAll(_directBrowserSlots);
        _directBrowserSlots = browserSlots;
        _includedBrowserSlots.clear();

        Iterator i = _activeClsWidgetDescriptors.values().iterator();
        while (i.hasNext()) {
            WidgetDescriptor d = (WidgetDescriptor) i.next();
            if (d.isIncluded()) {
                d.setIncluded(false);
            }
        }

        _cachedIncludedProjectFiles.clear();
        setProjectSlotValue(SLOT_INCLUDED_PROJECTS, null);
    }

    public void removeJavaPackageName(String packageName) {
        removeProjectSlotValue(SLOT_JAVA_PACKAGES, packageName);
        _domainKB.removeJavaLoadPackage(packageName);
    }

    public void removeProjectListener(ProjectListener listener) {
        _listeners.remove(this, listener);
    }

    private void removeProjectSlotValue(String slotName, Object value) {
        ModelUtilities.removeOwnSlotValue(_projectInstance, slotName, value);
    }

    private static void removeUnreferencedInstances(KnowledgeBase kb) {
        Instance projectInstance = getProjectInstance(kb);
        Collection roots = CollectionUtilities.createCollection(projectInstance);

        Iterator i = kb.getUnreachableSimpleInstances(roots).iterator();
        while (i.hasNext()) {
            Instance instance = (Instance) i.next();
            if (instance.isEditable()) {
                kb.deleteInstance(instance);
            }
        }
    }

    public void save(Collection errors) {
        /* save domain information first in case there is a problem with the project file
         * We would rather lose project information than domain information.  In addition,
         * this gives the backend a chance to hack the project on save.
         */
        saveDomainKB(errors);
        if (errors.isEmpty()) {
            flushProjectKBCache();
            makeTemporaryWidgetsIncluded(true);
            // removeUnreferencedInstances();  moved to load time
            saveProjectKB(errors);
            makeTemporaryWidgetsIncluded(false);
        }
        if (errors.isEmpty()) {
            _projectKB.setChanged(false);
            _domainKB.setChanged(false);
            setChanged(false);
            postProjectEvent(ProjectEvent.PROJECT_SAVED);
        }
    }

    private void saveBrowserSlots() {
        PropertyList browserSlots = getPropertyList(SLOT_BROWSER_SLOTS);
        browserSlots.clear();
        Iterator i = _directBrowserSlots.keySet().iterator();
        while (i.hasNext()) {
            Cls cls = (Cls) i.next();
            Slot slot = (Slot) _directBrowserSlots.get(cls);
            if (!isIncludedBrowserSlot(cls, slot)) {
                browserSlots.setString(cls.getName(), slot.getName());
            }
        }
    }

    private void saveCustomizedWidgets() {
        setProjectSlotValue(SLOT_CUSTOMIZED_INSTANCE_WIDGETS, null);
        Iterator i = _activeClsWidgetDescriptors.values().iterator();
        while (i.hasNext()) {
            WidgetDescriptor d = (WidgetDescriptor) i.next();
            if (!d.isTemporary()) {
                // Log.trace("addValue " + d.getName(), this, "saveCustomizedWidgets");
                addProjectSlotValue(SLOT_CUSTOMIZED_INSTANCE_WIDGETS, d.getInstance());
            } else {
                // Log.trace("discarding " + d.getName(), this, "saveCustomizedWidgets");
                // i.remove();
                // itsProjectKB.deleteInstance(d.getInstance());
            }
        }
    }

    private void saveDefaultMetaclasses() {
        setProjectSlotValue(SLOT_DEFAULT_CLS_METACLASS, _domainKB.getDefaultClsMetaCls().getName());
        setProjectSlotValue(SLOT_DEFAULT_SLOT_METACLASS, _domainKB.getDefaultSlotMetaCls().getName());
        setProjectSlotValue(SLOT_DEFAULT_FACET_METACLASS, _domainKB.getDefaultFacetMetaCls().getName());
    }

    private void saveDomainKB(Collection errors) {
        KnowledgeBaseFactory factory = getKnowledgeBaseFactory();
        if (factory != null) {
            factory.saveKnowledgeBase(_domainKB, getSources(), errors);
        }
    }

    private void saveHiddenClassFlags() {
        Collection hiddenClasses = new ArrayList();
        Iterator i = _hiddenFrames.iterator();
        while (i.hasNext()) {
            Cls cls = (Cls) i.next();
            if (!_includedHiddenFrames.contains(cls)) {
                hiddenClasses.add(cls.getName());
            }
        }
        setProjectSlotValues(SLOT_HIDDEN_CLASSES, hiddenClasses);
    }

    private void saveNextFrameNumber() {
        int number = _domainKB.getNextFrameNumber();
        setProjectSlotValue(SLOT_NEXT_FRAME_NUMBER, new Integer(number));
    }

    private void saveProjectKB(Collection errors) {
        new ClipsKnowledgeBaseFactory().saveKnowledgeBase(_projectKB, null, _filePath, errors);
    }

    private void savePropertyMap() {
        Instance propertyMapInstance = (Instance) getOwnSlotValue(_projectInstance, SLOT_PROPERTY_MAP);
        if (propertyMapInstance == null) {
            Cls cls = _projectKB.getCls(CLASS_MAP);
            propertyMapInstance = _projectKB.createInstance(null, cls);
            ModelUtilities.addOwnSlotValue(_projectInstance, SLOT_PROPERTY_MAP, propertyMapInstance);
        }
        PropertyMapUtil.store(_propertyMap, propertyMapInstance);
    }

    private void saveTabWidgetInstances() {
        Collection instances = new ArrayList();
        Iterator i = _tabWidgetDescriptors.iterator();
        while (i.hasNext()) {
            WidgetDescriptor d = (WidgetDescriptor) i.next();
            String clsName = d.getWidgetClassName();
            if (SystemUtilities.forName(clsName) != null) {
                instances.add(d.getInstance());
            }
        }
        setProjectSlotValues(SLOT_TABS, instances);
    }

    private void setChanged(boolean b) {
        // Log.stack("***", this, "setChanged", new Boolean(b));
        _hasChanged = b;
    }

    public void setDirectBrowserSlot(Cls cls, Slot slot) {
        recordDirectBrowserSlot(cls, slot);
        setChanged(true);
    }

    public void setDisplayAbstractClassIcon(boolean b) {
        _displayAbstractClassIcon = new Boolean(b);
        setOption(SLOT_DISPLAY_ABSTRACT_CLASS_ICON, b);
    }

    public void setDisplayConfirmationOnRemove(boolean b) {
        _displayConfirmationOnRemove = new Boolean(b);
        setOption(SLOT_DISPLAY_REMOVE_CONFIRMATION_DIALOG, b);
    }

    public void setDisplayHiddenClasses(boolean b) {
        _displayHiddenClasses = new Boolean(b);
        setOption(SLOT_DISPLAY_HIDDEN_CLASSES, b);
    }

    public void setDisplayMultiParentClassIcon(boolean b) {
        _displayMultiParentClassIcon = new Boolean(b);
        setOption(SLOT_DISPLAY_MULTI_PARENT_CLASS_ICON, b);
    }

    public void setHidden(Frame frame, boolean hidden) {
        recordHidden(frame, hidden);
        setChanged(true);
    }

    public void setIsReadonly(boolean b) {
        _isReadonly = new Boolean(b);
        setOption(SLOT_IS_READONLY, b);
    }

    public void setJournalingEnabled(boolean enable) {
        if (isJournalingEnabled() != enable) {
            setProjectSlotValue(SLOT_JOURNALING_ENABLED, new Boolean(enable));
            if (enable) {
                Journal.setJournalFile(getJournalFile());
                Journal.startRecording();
                Journal.enter(this, "journaling enabled");
            } else {
                Journal.enter(this, "journaling disabled");
                Journal.stopRecording();
            }
        }
    }

    public void setKnowledgeBaseFactory(KnowledgeBaseFactory factory) {
        Assert.assertNotNull("factory", factory);
        getSources().setString(KnowledgeBaseFactory.FACTORY_CLASS_NAME, factory.getClass().getName());
    }

    private void setLocation(Window window) {
        if (_lastLocation == null) {
            ComponentUtilities.center(window);
            _lastLocation = window.getLocation();
        } else {
            _lastLocation.x += 25;
            _lastLocation.y += 25;
            Dimension screenSize = window.getToolkit().getScreenSize();

            if (_lastLocation.x + window.getWidth() > screenSize.width ||
                    _lastLocation.y + window.getHeight() > screenSize.height) {
                _lastLocation = new Point();
            }
            window.setLocation(_lastLocation);
        }
    }

    private void setOption(String slotName, boolean value) {
        setOwnSlotValue(getOptionsInstance(), slotName, new Boolean(value));
    }

    private static void setOwnSlotValue(Frame frame, String slotName, Object value) {
        ModelUtilities.setOwnSlotValue(frame, slotName, value);
    }

    public void setProjectFilePath(String path) {
        // Log.enter(this, "setProjectFilePath", path);
        if (path == null || !path.equals(_filePath)) {
            _filePath = path;
            FileUtilities.setCurrentWorkingDirectoryFromFile(_filePath);
            updateKBNames();
            updateJournaling();
            updateIncludeFilePaths();
        }
    }

    private void setProjectSlotValue(String slotName, Object value) {
        ModelUtilities.setOwnSlotValue(_projectInstance, slotName, value);
    }

    private void setProjectSlotValues(String slotName, Collection values) {
        ModelUtilities.setOwnSlotValues(_projectInstance, slotName, values);
    }

    public void setTabWidgetDescriptorOrder(Collection c) {
        _tabWidgetDescriptors = new ArrayList(c);
        saveTabWidgetInstances();
    }

    public void setUpdateModificationSlots(boolean b) {
        _updateModificationSlots = new Boolean(b);
        setOption(SLOT_UPDATE_MODIFICATION_SLOTS, b);
    }

    private void setupJournaling() {
        if (isJournalingEnabled()) {
            String journalFile = getJournalFile();
            Journal.setJournalFile(journalFile);
            Journal.startRecording();
        }
    }

    public JFrame show(Cls cls, Slot slot) {
        FrameSlotCombination combination = new FrameSlotCombination(cls, slot);
        JFrame frame = (JFrame) _frames.get(combination);
        if (frame == null) {
            frame = createFrame(cls, slot);
            frame.setIconImage(Icons.getSlotImage());
        } else {
            frame.toFront();
            frame.requestFocus();
        }
        return frame;
    }

    public JFrame show(Instance instance) {
        Assert.assertNotNull("instance", instance);
        JFrame frame = (JFrame) _frames.get(instance);
        if (frame == null) {
            frame = createFrame(instance);
            frame.setIconImage(getImage(instance));
        } else {
            frame.toFront();
            frame.requestFocus();
        }
        return frame;
    }

    public JFrame show(String instanceName) {
        Assert.assertNotNull("instance name", instanceName);
        Instance instance = _domainKB.getInstance(instanceName);
        return show(instance);
    }

    public JInternalFrame showInInternalFrame(Instance instance) {
        Assert.assertNotNull("instance", instance);
        InstanceDisplay display = new InstanceDisplay(this, false, false);
        display.setInstance(instance);
        String title = display.getCurrentClsWidget().getLabel();
        JInternalFrame frame = new JInternalFrame(title, true);
        frame.getContentPane().setLayout(new BorderLayout());
        frame.getContentPane().add(display);
        frame.setFrameIcon(Icons.getNerd16x16Icon());
        frame.pack();

        return frame;
    }

    public String toString() {
        return "Project(" + getName() + ")";
    }

    private void updateIncludeFilePaths() {
        // Collection updatedPaths = new ArrayList();
        // setProjectSlotValues(SLOT_INCLUDED_PROJECTS, updatedPaths);
    }

    private void updateJournaling() {
        if (_projectKB != null && isJournalingEnabled()) {
            Journal.stopRecording();
            Journal.setJournalFile(getJournalFile());
            Journal.startRecording();
        }
    }

    private void updateKBNames() {
        if (_filePath != null) {
            if (_domainKB != null) {
                _domainKB.setName(getName());
            }
            if (_projectKB != null) {
                _projectKB.setName(getName() + "_ProjectKB");
            }
        }
    }

    private WidgetDescriptor getClsWidgetDescriptor(Cls cls) {
        WidgetDescriptor d = (WidgetDescriptor) _activeClsWidgetDescriptors.get(cls);
        if (d == null) {
            d = WidgetDescriptor.create(_projectKB);
            d.setWidgetClassName(_defaultInstanceWidgetClassName);
            d.setName(cls.getName());
            d.setTemporary(true);
            _activeClsWidgetDescriptors.put(cls, d);
        }
        return d;
    }
}
