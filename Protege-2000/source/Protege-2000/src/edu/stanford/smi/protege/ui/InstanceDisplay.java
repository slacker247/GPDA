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

package edu.stanford.smi.protege.ui;


import java.awt.*;
import java.awt.event.*;
import java.text.*;
import java.util.*;
import javax.swing.*;
import javax.swing.event.*;
import edu.stanford.smi.protege.event.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.resource.*;
import edu.stanford.smi.protege.widget.*;
import edu.stanford.smi.protege.util.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class InstanceDisplay extends JDesktopPane implements Disposable {
    private Project _project;
    private JScrollPane _scrollPane;
    private ClsWidget _currentWidget;
    private Instance _currentInstance;
    private JLabel _headerLabel;
    private JComponent _child;
    // private AbstractButton _hideStickiesButton;
    private Point _lastYellowStickyPosition = new Point();

    private ClsListener _clsListener = new ClsAdapter() {
        public void directSuperclassAdded(ClsEvent event) {
            reloadForm();
        }

        public void directSuperclassRemoved(ClsEvent event) {
            reloadForm();
        }

        public void templateSlotAdded(ClsEvent event) {
            reloadForm();
        }

        public void templateSlotRemoved(ClsEvent event) {
            reloadForm();
        }

        public void templateFacetValueChanged(ClsEvent event) {
            reloadForm();
        }
    };

    private WidgetListener _widgetListener = new WidgetAdapter() {
        public void labelChanged(WidgetEvent event) {
            if (_headerLabel != null) {
                loadHeader();
            }
        }
    };

    private ProjectListener _projectListener = new ProjectAdapter() {
        public void formChanged(ProjectEvent event) {
            Cls cls = event.getCls();
            if (_currentWidget != null && _currentWidget.getCls() == cls) {
                reloadForm();
            }
        }
    };

    private InstanceListener _instanceListener = new InstanceListener() {
        public void directTypeChanged(InstanceEvent event) {
            setInstance(_currentWidget.getInstance());
        }
    };

    public InstanceDisplay(Project project) {
        this(project, true, true);
    }

    public InstanceDisplay(Project project, boolean showHeader, boolean showHeaderLabel) {
        _child = new JPanel(new BorderLayout());
        if (showHeader) {
            _child.add(createHeader(), BorderLayout.NORTH);
            if (!showHeaderLabel) {
                _headerLabel.setVisible(false);
            }
        }
        this._project = project;
        project.addProjectListener(_projectListener);
        _scrollPane = new JScrollPane();
        _child.add(_scrollPane, BorderLayout.CENTER);
        add(_child);
    }

    private void addRuntimeWidget(Instance instance) {
        Cls type = instance.getDirectType();
        if (type == null) {
            Log.warning("instance has no type", this, "Instance", instance);
        } else {
            type.addClsListener(_clsListener);
            _currentWidget = _project.createRuntimeClsWidget(instance);
            _currentWidget.addWidgetListener(_widgetListener);
            JComponent component = (JComponent) _currentWidget;
            _scrollPane.setViewportView(component);
            update();
        }
    }

    private void checkForValidAnnotation(Instance instance) {
        if (ModelUtilities.getOwnSlotValue(instance, Model.Slot.ANNOTATION_TEXT) == null) {
            instance.getKnowledgeBase().deleteInstance(instance);
        }
    }

    private Action createCreateYellowStickiesAction() {
        return new AbstractAction("Create Note", Icons.getCreateYellowStickyIcon()) {
            public void actionPerformed(ActionEvent event) {
                createYellowSticky();
            }
        };
    }

    private Action createDeleteYellowStickiesAction() {
        return new AbstractAction("Delete Note", Icons.getDeleteYellowStickyIcon()) {
            public void actionPerformed(ActionEvent event) {
                deleteYellowSticky();
            }
        };
    }

    private JComponent createHeader() {
        JComponent header = new JPanel();
        header.setLayout(new BorderLayout());
        _headerLabel = new JLabel();
        header.add(_headerLabel, BorderLayout.CENTER);
        FakeToolBar toolBar = ComponentFactory.createFakeToolBar(ComponentFactory.SMALL_BUTTON_SIZE);
        JButton button = ComponentFactory.addToolBarButton(toolBar, createCreateYellowStickiesAction());
        button.setBackground(new Color(255, 255, 204));
        button = ComponentFactory.addToolBarButton(toolBar, createDeleteYellowStickiesAction());
        button.setBackground(new Color(255, 255, 204));
        header.add(toolBar, BorderLayout.EAST);
        return header;
    }

    private Action createHideYellowStickiesAction() {
        return new AbstractAction("Hide Notes", Icons.getViewYellowStickiesIcon()) {
            public void actionPerformed(ActionEvent event) {
                updateStickies();
            }
        };
    }

    private void createYellowSticky() {
        ensureYellowStickiesAreVisible();
        KnowledgeBase kb = _project.getKnowledgeBase();
        Instance instance = kb.createInstance(null, kb.getCls(Model.Cls.INSTANCE_ANNOTATION));
        ModelUtilities.setOwnSlotValue(instance, Model.Slot.CREATOR, Journal.getUserName());
        DateFormat formatter = new StandardDateFormat();
        String date = formatter.format(new Date());
        ModelUtilities.setOwnSlotValue(instance, Model.Slot.CREATION_TIMESTAMP, date);
        ModelUtilities.setOwnSlotValue(instance, Model.Slot.ANNOTATED_INSTANCE, _currentInstance);
        showYellowSticky(instance);
    }

    private void deleteYellowSticky() {
        Collection stickyInstances = getStickyInstances();
        int count = stickyInstances.size();
        if (count == 1) {
            String text = "Are you sure that you want to delete this note";
            int result = ModalDialog.showMessageDialog(this, text, ModalDialog.MODE_YES_NO);
            if (result == ModalDialog.OPTION_YES) {
                Instance instance = (Instance) CollectionUtilities.getFirstItem(stickyInstances);
                removeSticky(instance);
                instance.delete();
            }
        } else if (count > 1) {
            Collection c = DisplayUtilities.pickInstancesFromCollection(this, stickyInstances, "Select a note to delete");
            Iterator i = c.iterator();
            while (i.hasNext()) {
                Instance instance = (Instance) i.next();
                removeSticky(instance);
                instance.delete();
            }
        }
    }

    public void dispose() {
        _project.removeProjectListener(_projectListener);
        if (_currentInstance != null) {
            _currentInstance.removeInstanceListener(_instanceListener);
        }
        if (_currentWidget != null) {
            _currentWidget.removeWidgetListener(_widgetListener);
            _currentWidget.getCls().removeClsListener(_clsListener);
        }
    }

    private void ensureYellowStickiesAreVisible() {
    }

    public ClsWidget getCurrentClsWidget() {
        return _currentWidget;
    }

    public Instance getCurrentInstance() {
        return _currentInstance;
    }

    private Point getNextYellowStickyPosition() {
        int OFFSET = 25;
        int MAX_OFFSET = 100;

        _lastYellowStickyPosition.x += OFFSET;
        _lastYellowStickyPosition.x %= MAX_OFFSET;

        _lastYellowStickyPosition.y += OFFSET;
        _lastYellowStickyPosition.y %= MAX_OFFSET;

        return _lastYellowStickyPosition;
    }

    public Dimension getPreferredSize() {
        return _child.getPreferredSize();
    }

    private Collection getStickyInstances() {
        Collection stickyInstances = new ArrayList();
        if (_currentInstance != null) {
            KnowledgeBase kb = _project.getKnowledgeBase();
            /*
            Slot annotationSlot = kb.getSlot(Model.Slot.ANNOTATED_INSTANCE);
            Collection refs = kb.getReferences(_currentInstance, 0);
            Iterator i = refs.iterator();
            Log.trace("references=" + refs.size(), this, "getStickyInstances");
            while (i.hasNext()) {
                Reference ref = (Reference) i.next();
                if (ref.getSlot() == annotationSlot) {
                    stickyInstances.add(ref.getFrame());
                }
            }
            */
            Slot annotationSlot = kb.getSlot(Model.Slot.ANNOTATED_INSTANCE);
            Iterator i = kb.getCls(Model.Cls.INSTANCE_ANNOTATION).getInstances().iterator();
            while (i.hasNext()) {
                Instance annotationInstance = (Instance) i.next();
                Instance pointedAtInstance = (Instance) annotationInstance.getOwnSlotValue(annotationSlot);
                if (pointedAtInstance == _currentInstance) {
                    stickyInstances.add(annotationInstance);
                }
            }
        }
        return stickyInstances;
    }

    private Map getYellowStickyMap() {
        String mapName = "InstanceDisplay.yellow_stickies";
        Map map = (Map) _project.getPropertyMap().get(mapName);
        if (map == null) {
            map = new HashMap();
            _project.getPropertyMap().put(mapName, map);
        }
        return map;
    }

    private void loadHeader() {
        Icon icon = null;
        String text = "";
        if (_currentWidget != null) {
            Instance instance = _currentWidget.getInstance();
            text = _currentWidget.getLabel();
            if (instance instanceof Cls) {
                if (instance.isEditable()) {
                    icon = Icons.getClsIcon();
                } else {
                    icon = Icons.getReadonlyClsIcon();
                }
            } else if (instance instanceof Slot) {
                icon = Icons.getSlotIcon();
            } else if (instance instanceof Facet) {
                icon = Icons.getFacetIcon();
            } else {
                if (instance.isEditable()) {
                    icon = Icons.getInstanceIcon();
                } else {
                    icon = Icons.getReadonlyInstanceIcon();
                }
            }
        }
        _headerLabel.setIcon(icon);
        _headerLabel.setText(text);
    }

    private JInternalFrame loadIntoFrame(final Instance instance) {
        JInternalFrame frame = _project.showInInternalFrame(instance);
        Map propertyMap = getYellowStickyMap();
        Rectangle r = (Rectangle) propertyMap.get(instance);
        if (r == null) {
            frame.setLocation(getNextYellowStickyPosition());
            propertyMap.put(instance, frame.getBounds());
        } else {
            frame.setBounds(r);
        }
        frame.addComponentListener(new ComponentAdapter() {
            public void componentResized(ComponentEvent event) {
                getYellowStickyMap().put(instance, event.getComponent().getBounds());
            }
            public void componentMoved(ComponentEvent event) {
                getYellowStickyMap().put(instance, event.getComponent().getBounds());
            }
        });
        /*
        frame.addInternalFrameListener(new InternalFrameAdapter() {
            public void internalFrameClosed(InternalFrameEvent event) {
                ComponentUtilities.dispose((Component) event.getSource());
                // Log.enter(this, "internalFrameClosed", event);
                checkForValidAnnotation(instance);
            }
        });
        */
        return frame;
    }

    private void reloadForm() {
        Instance instance = _currentWidget.getInstance();
        removeCurrentWidget();
        setInstance(instance);
    }

    private void removeAllStickies() {
        Iterator i = new ArrayList(Arrays.asList(getComponents())).iterator();
        while (i.hasNext()) {
            Component c = (Component) i.next();
            if (c instanceof JInternalFrame) {
                JInternalFrame frame = (JInternalFrame) c;
                frame.setVisible(false);
                frame.dispose();
                remove(frame);
            }
        }
    }

    private void removeCurrentWidget() {
        _currentWidget.getCls().removeClsListener(_clsListener);
        _currentWidget.removeWidgetListener(_widgetListener);
        Component c = (Component) _currentWidget;
        _scrollPane.setViewportView(null);
        ComponentUtilities.dispose(c);
        _currentWidget = null;
        _currentInstance = null;
        update();
    }

    private void removeSticky(Instance instance) {
        Iterator i = new ArrayList(Arrays.asList(getComponents())).iterator();
        while (i.hasNext()) {
            Component c = (Component) i.next();
            if (c instanceof JInternalFrame) {
                JInternalFrame frame = (JInternalFrame) c;
                InstanceDisplay display = (InstanceDisplay) frame.getContentPane().getComponent(0);
                if (display.getCurrentInstance() == instance) {
                    frame.setVisible(false);
                    frame.dispose();
                    remove(frame);
                    break;
                }
            }
        }
    }

    public void reshape(int x, int y, int w, int h) {
        super.reshape(x, y, w, h);
        _child.setBounds(0, 0, w, h);
    }

    public void setInstance(Instance instance) {
        // if (instance != _currentInstance) {
        if (_currentInstance != null) {
            _currentInstance.removeInstanceListener(_instanceListener);
        }
        if (instance == null) {
            if (_currentWidget != null) {
                removeCurrentWidget();
            }
        } else {
            if (_currentWidget == null) {
                addRuntimeWidget(instance);
            } else {
                if (_currentWidget.getCls() == instance.getDirectType()) {
                    _currentWidget.setInstance(instance);
                } else {
                    removeCurrentWidget();
                    addRuntimeWidget(instance);
                }
            }
            instance.addInstanceListener(_instanceListener);
        }
        _currentInstance = instance;
        if (_headerLabel != null) {
            loadHeader();
        }
        updateStickies();
        // }
    }

    private void showAllStickies() {
        if (_currentInstance != null) {
            Iterator i = getStickyInstances().iterator();
            while (i.hasNext()) {
                Instance instance = (Instance) i.next();
                showYellowSticky(instance);
            }
        }
    }

    private void showYellowSticky(Instance instance) {
        JInternalFrame frame = loadIntoFrame(instance);
        String author = (String) ModelUtilities.getOwnSlotValue(instance, Model.Slot.CREATOR);
        if (author == null || author.length() == 0)
            author = "<unknown>";
        String timestamp = (String) ModelUtilities.getOwnSlotValue(instance, Model.Slot.CREATION_TIMESTAMP);
        SimpleDateFormat formatter = new StandardDateFormat();
        Date date;
        try {
            date = formatter.parse(timestamp);
        } catch (ParseException e) {
            date = new Date();
            Log.exception(e, this, "showYellowSticky", instance);
        }
        Calendar calendar = new GregorianCalendar();
        int thisYear = calendar.get(Calendar.YEAR);
        calendar.setTime(date);
        int stickyYear = calendar.get(Calendar.YEAR);
        String pattern = "MMM dd " + ((thisYear == stickyYear) ? "" : "yyyy ") + "HH:mm";
        formatter.applyPattern(pattern);
        String timeString = formatter.format(date);
        String title = author + ", " + timeString;
        frame.setTitle(title);
        frame.setVisible(true);
        add(frame);
        frame.toFront();
        try {
            frame.setSelected(true);
        } catch (Exception e) {
        }
    }

    private void update() {
        revalidate();
        repaint();
    }

    private void updateStickies() {
        removeAllStickies();
        showAllStickies();
    }
}
