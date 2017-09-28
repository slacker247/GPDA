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
import java.util.*;
import javax.swing.*;
import edu.stanford.smi.protege.event.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.model.Frame;
import edu.stanford.smi.protege.widget.*;
import edu.stanford.smi.protege.util.*;

/**
 * Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class FormWidget extends AbstractClsWidget {
    private final static String PROPERTY_LAYOUT_PROPERTIES = "layout properties";
    private final static String PROPERTY_HORIZONTAL_STRETCHER = "horizontal_stretcher";
    private final static String PROPERTY_VERTICAL_STRETCHER = "vertical_stretcher";
    private final static int RESIZE_NONE = Cursor.DEFAULT_CURSOR;
    private final static int RESIZE_NW = Cursor.NW_RESIZE_CURSOR;
    private final static int RESIZE_N = Cursor.N_RESIZE_CURSOR;
    private final static int RESIZE_NE = Cursor.NE_RESIZE_CURSOR;
    private final static int RESIZE_E = Cursor.E_RESIZE_CURSOR;
    private final static int RESIZE_SE = Cursor.SE_RESIZE_CURSOR;
    private final static int RESIZE_S = Cursor.S_RESIZE_CURSOR;
    private final static int RESIZE_SW = Cursor.SW_RESIZE_CURSOR;
    private final static int RESIZE_W = Cursor.W_RESIZE_CURSOR;

    private WidgetLayoutStrategy _cachedLayoutStrategy = new DefaultWidgetLayoutStrategy();
    private AbstractSlotWidget _selectedWidget;
    private Dimension _cursorOffset;
    private int _resizeDirection = RESIZE_NONE;
    private int _gridSize = 10;
    private boolean _isDragging;

    private KeyListener _widgetKeyListener = new KeyAdapter() {
        public void keyPressed(KeyEvent event) {
            Component c = event.getComponent();
            int key = event.getKeyCode();
            switch (key) {
                case KeyEvent.VK_LEFT :
                    translate(c, -1, 0);
                    break;
                case KeyEvent.VK_RIGHT :
                    translate(c, 1, 0);
                    break;
                case KeyEvent.VK_UP :
                    translate(c, 0, -1);
                    break;
                case KeyEvent.VK_DOWN :
                    translate(c, 0, 1);
                    break;
                default :
                    // do nothing
                    break;
            }
        }
    };
    private MouseListener _formMouseListener = new MouseAdapter() {
        public void mousePressed(MouseEvent e) {
            mousePressedOnForm(e.getPoint());
        }
        public void mouseDragged(MouseEvent e) {
            mouseDraggedOnForm(e.getPoint());
        }
        public void mouseClicked(MouseEvent e) {
            if (e.getClickCount() == 2) {
                configure();
            }
        }
    };
    private MouseMotionListener _formMouseMotionListener = new MouseMotionAdapter() {
        public void mouseMoved(MouseEvent e) {
            mouseMovedOnForm(e.getPoint());
        }
    };
    private MouseListener _widgetMouseListener = new MouseAdapter() {
        public void mousePressed(MouseEvent e) {
            mousePressedOnWidget(e.getComponent(), e.getPoint());
        }
        public void mouseReleased(MouseEvent e) {
            mouseReleasedOnWidget(e.getComponent(), e.getPoint());
        }
        public void mouseClicked(MouseEvent e) {
            mouseClickedOnWidget(e.getComponent(), e.getPoint(), e.getClickCount());
        }
    };
    private MouseMotionListener _widgetMouseMotionListener = new MouseMotionListener() {
        public void mouseDragged(MouseEvent e) {
            mouseDraggedOnWidget(e.getComponent(), e.getPoint());
        }
        public void mouseMoved(MouseEvent e) {
            mouseMovedOnWidget(e.getComponent(), e.getPoint());
        }
    };
    private ClsListener _clsListener = new ClsAdapter() {
        public void templateSlotAdded(ClsEvent event) {
            addWidget(event.getSlot());
        }
        public void templateSlotRemoved(ClsEvent event) {
            removeWidget(event.getSlot());
        }
        public void templateFacetValueChanged(ClsEvent event) {
            updateWidget(event.getSlot());
        }
    };
    private KnowledgeBaseListener _kbListener = new KnowledgeBaseAdapter() {
        public void frameNameChanged(KnowledgeBaseEvent event) {
            Frame frame = event.getFrame();
            if (frame instanceof Slot) {
                Slot slot = (Slot) frame;
                WidgetDescriptor d = getPropertyList().getWidgetDescriptor(event.getOldName());
                if (d != null) {
                    d.setName(frame.getName());
                    updateWidget(slot);
                }
            }
        }
    };

    public FormWidget() {
    }

    private void addWidget(Slot slot) {
        int count = getCustomizedComponentCount();
        Widget widget = createWidget(slot);
        layoutWidgets(count);
    }

    private static void adjustEast(Rectangle r, Point p) {
        r.width -= r.x + r.width - p.x;
    }

    private static void adjustNorth(Rectangle r, Point p) {
        r.height += r.y - p.y;
        r.y = p.y;
    }

    private static void adjustSouth(Rectangle r, Point p) {
        r.height -= r.y + r.height - p.y;
    }

    private static void adjustWest(Rectangle r, Point p) {
        r.width += r.x - p.x;
        r.x = p.x;
    }

    private boolean canRemoveCustomizations() {
        FrameID id = getCls().getFrameID();
        return !(id == Model.Cls.ID.STANDARD_CLASS || id == Model.Cls.ID.STANDARD_SLOT);
    }

    public void clearSelection() {
        if (_selectedWidget != null) {
            _selectedWidget.setNormalBorder();
            _selectedWidget = null;
            notifySelectionListeners();
        }
    }

    public boolean configure() {
        FormWidgetConfigurationPanel p = new FormWidgetConfigurationPanel(this);
        int result = ModalDialog.showDialog(this, p, "Configure form for " + getCls().getName(), ModalDialog.MODE_OK_CANCEL);
        boolean configured = result == ModalDialog.OPTION_OK;
        if (configured) {
            setModified(true);
        }
        return configured;
    }

    public void createDescriptorsAndWidgets(Collection slots) {
        Iterator i = slots.iterator();
        while (i.hasNext()) {
            Slot slot = (Slot) i.next();
            createWidget(slot);
        }
    }

    private Widget createWidget(Slot slot) {
        return createWidget(slot, null);
    }

    private Widget createWidget(Slot slot, String widgetClassName) {
        // Log.enter(this, "createWidget", slot);
        WidgetDescriptor d = createWidgetDescriptor(slot);
        if (widgetClassName != null) {
            d.setWidgetClassName(widgetClassName);
        }
        return createWidget(d);
    }

    private SlotWidget createWidget(WidgetDescriptor descriptor) {
        SlotWidget widget;
        if (descriptor.isNull()) {
            widget = null;
        } else {
            KnowledgeBase kb = getKnowledgeBase();
            String slotName = descriptor.getName();
            Slot slot = kb.getSlot(slotName);
            widget = WidgetUtilities.createSlotWidget(descriptor, isDesignTime(), getProject(), getCls(), slot);
            setupWidget(widget);
        }
        return widget;
    }

    public WidgetDescriptor createWidgetDescriptor(Slot slot) {
        WidgetDescriptor d = getProject().createWidgetDescriptor(getCls(), slot, null);
        getPropertyList().setWidgetDescriptor(d);
        // Log.trace("insert in pl: " + d.getName(), this, "createWidgetDescriptor", slot);
        return d;
    }

    private void createWidgets() {
        Collection slots = new HashSet(getCls().getTemplateSlots());
        Iterator i = getPropertyList().getNames().iterator();
        while (i.hasNext()) {
            String name = (String) i.next();
            // Log.trace("creating: " + name, this, "createWidgets");
            if (isSlotName(name)) {
                WidgetDescriptor descriptor = getPropertyList().getWidgetDescriptor(name);
                if (descriptor != null) {
                    String slotName = descriptor.getName();
                    boolean succeeded = true;
                    if (slotName != null) {
                        Slot slot = getKnowledgeBase().getSlot(slotName);
                        succeeded = isSuitable(descriptor, getCls(), slot);
                        if (succeeded) {
                            succeeded = slots.remove(slot);
                        }
                    }
                    if (succeeded) {
                        createWidget(descriptor);
                    } else {
                        // Log.trace("removing out of date slot widget: " + slotName, this, "createWidgets");
                        getPropertyList().remove(name);
                    }
                }
            }
        }
        int startIndex = getCustomizedComponentCount();
        createDescriptorsAndWidgets(slots);
        layoutWidgets(startIndex);
        doLayoutSanityCheck();
    }

    public void dispose() {
        super.dispose();
        getKnowledgeBase().removeKnowledgeBaseListener(_kbListener);
        if (isDesignTime()) {
            removeMouseListener(_formMouseListener);
            removeMouseMotionListener(_formMouseMotionListener);
            getCls().removeClsListener(_clsListener);
        }
    }

    private void doLayoutSanityCheck() {
        if (true || isDesignTime()) {
            boolean topLeftAlreadyPresent = false;
            // make sure that components stacked in the top-left corner (because of previous bugs) are visible
            int n = getComponentCount();
            for (int i = 0; i < n; ++i) {
                Component c = getComponent(i);
                Point p = c.getLocation();
                if (p.x == 0 && p.y == 0) {
                    if (topLeftAlreadyPresent) {
                        Log.trace("moving hidden widget: " + c, this, "doLayoutSanityCheck");
                        // just move it to the end
                        c.setLocation(0, getPreferredSize().height);
                        revalidate();
                    } else {
                        topLeftAlreadyPresent = true;
                    }
                }

            }
        }
    }

    private void dragWidget(JComponent widget, Point point) {
        point.translate(_cursorOffset.width, _cursorOffset.height);
        Point gridPoint = gridPoint(point);
        if (!gridPoint.equals(widget.getLocation())) {
            widget.setLocation(gridPoint);
        }
    }

    private Component getComponent(Slot slot) {
        Component component = null;
        int nComponents = getComponentCount();
        for (int i = 0; i < nComponents; ++i) {
            Component c = getComponent(i);
            if (c instanceof SlotWidget) {
                SlotWidget w = (SlotWidget) c;
                if (w.getSlot() == slot) {
                    component = c;
                    break;
                }
            }
        }
        return component;
    }

    private int getCustomizedComponentCount() {
        return getComponentCount();
    }

    private static Cls getCustomizedParent(Cls cls) {
        Project project = cls.getProject();
        Cls customizedParent = null;
        Iterator i = cls.getDirectSuperclasses().iterator();
        while (i.hasNext() && customizedParent == null) {
            Cls parent = (Cls) i.next();
            if (project.hasCustomizedDescriptor(parent)) {
                customizedParent = parent;
                break;
            }
            customizedParent = getCustomizedParent(parent);
            break;
        }
        return customizedParent;
    }

    public Slot getHorizontalStretcher() {
        return getLayoutSlot(PROPERTY_HORIZONTAL_STRETCHER);
    }

    public String getLabel() {
        String label;
        if (isDesignTime()) {
            label = getCls().getName();
        } else {
            Instance instance = getInstance();
            Slot slot = getSlot();
            if (instance instanceof Cls && slot != null) {
                label = slot.getBrowserText() + " at class " + instance.getBrowserText();
            } else {
                label = instance.getBrowserText();
                String typeName = instance.getDirectType().getBrowserText();
                if (!typeName.equals(Model.Cls.STANDARD_CLASS) && !typeName.equals(Model.Cls.STANDARD_SLOT)) {
                    label += " (" + typeName + ")";
                }
            }
        }
        return label;
    }

    private PropertyList getLayoutProperties() {
        return getPropertyList().getPropertyList(PROPERTY_LAYOUT_PROPERTIES);
    }

    private Slot getLayoutSlot(String propertyName) {
        String slotName = getLayoutProperties().getString(propertyName);
        Slot slot;
        if (slotName == null) {
            slot = null;
        } else {
            slot = getKnowledgeBase().getSlot(slotName);
        }
        return slot;
    }

    private Point getParentPoint(Point p) {
        return SwingUtilities.convertPoint(this, p, getParent());
    }

    public Dimension getPreferredSize() {
        Dimension preferredSize = new Dimension();
        Iterator i = getPropertyList().getNames().iterator();
        while (i.hasNext()) {
            String widgetName = (String) i.next();
            if (isSlotName(widgetName)) {
                WidgetDescriptor descriptor = getPropertyList().getWidgetDescriptor(widgetName);
                if (!descriptor.isNull()) {
                    Rectangle r = descriptor.getBounds();
                    if (r == null) {
                        Log.warning("null rectangle: " + widgetName, this, "getPreferredSize");
                    } else {
                        preferredSize.width = Math.max(preferredSize.width, r.x + r.width);
                        preferredSize.height = Math.max(preferredSize.height, r.y + r.height);
                    }
                }
            }
        }
        return preferredSize;
    }

    private int getResizeDirection(Component c, Point p) {
        int direction;
        Rectangle outerRect = c.getBounds();
        Rectangle innerRect = new Rectangle(outerRect);
        int BORDER_SIZE = AbstractSlotWidget.getSelectionBorderSize();
        innerRect.grow(-BORDER_SIZE, -BORDER_SIZE);
        if (innerRect.contains(p)) {
            direction = RESIZE_NONE;
        } else {
            if (p.x <= innerRect.x) {
                if (p.y <= innerRect.y) {
                    direction = RESIZE_NW;
                } else if (p.y >= innerRect.y + innerRect.height) {
                    direction = RESIZE_SW;
                } else {
                    direction = RESIZE_W;
                }
            } else if (p.x >= innerRect.x + innerRect.width) {
                if (p.y <= innerRect.y) {
                    direction = RESIZE_NE;
                } else if (p.y >= innerRect.y + innerRect.height) {
                    direction = RESIZE_SE;
                } else {
                    direction = RESIZE_E;
                }
            } else if (p.y <= innerRect.y) {
                direction = RESIZE_N;
            } else if (p.y >= innerRect.y + innerRect.height) {
                direction = RESIZE_S;
            } else {
                throw new IllegalStateException("Logic error on resize direction");
            }
        }
        return direction;
    }

    public Collection getSelection() {
        return CollectionUtilities.createCollection(_selectedWidget);
    }

    public Slot getVerticalStretcher() {
        return getLayoutSlot(PROPERTY_VERTICAL_STRETCHER);
    }

    private SlotWidget getWidget(Slot slot) {
        int componentCount = getComponentCount();
        SlotWidget widget = null;
        for (int i = 0; i < componentCount; ++i) {
            Component c = getComponent(i);
            if (c instanceof Widget) {
                SlotWidget w = (SlotWidget) c;
                if (w.getSlot() == slot) {
                    widget = w;
                    break;
                }
            }
        }
        return widget;
    }

    private JComponent getWidget(Component descendent) {
        JComponent widget = null;
        for (int i = 0; i < getComponentCount(); ++i) {
            JComponent c = (JComponent) getComponent(i);
            if (c == descendent || c.isAncestorOf(descendent)) {
                widget = c;
                break;
            }
        }
        Assert.assertNotNull("widget", widget);
        return widget;
    }

    public WidgetDescriptor getWidgetDescriptor(Slot slot) {
        return getPropertyList().getWidgetDescriptor(slot.getName());
    }

    private Point getWidgetPoint(Component c, Point p) {
        return SwingUtilities.convertPoint(c, p, this);
    }

    private Point gridPoint(Point p) {
        Point gridPoint = new Point(p);
        gridPoint.x -= gridPoint.x % _gridSize;
        gridPoint.y -= gridPoint.y % _gridSize;
        return gridPoint;
    }

    public boolean hasWidgetDescriptor(Slot slot) {
        return getWidgetDescriptor(slot) != null;
    }

    public boolean hasWidgetDescriptors() {
        boolean b = !getPropertyList().getNames().isEmpty();
        return b;
    }

    public void initialize() {
        setLayout(null);

        initializeWidgets();
        getKnowledgeBase().addKnowledgeBaseListener(_kbListener);
        if (isDesignTime()) {
            addMouseListener(_formMouseListener);
            addMouseMotionListener(_formMouseMotionListener);
            getCls().addClsListener(_clsListener);
        } else {
            setLayout(new ResizingLayout());
        }
        // hack
        setOpaque(true);
        setBackground(UIManager.getColor("Label.background"));
    }

    private void initializeWidgets() {
        if (isDesignTime() && !isModified()) {
            // force the widget to get laid out
            Cls parent = getCustomizedParent(getCls());
            if (parent != null) {
                layoutLikeCls(parent);
                setModified(false);
            } else {
                createWidgets();
            }
        } else {
            createWidgets();
        }
    }

    private boolean isCustomized() {
        return getDescriptor().isDirectlyCustomizedByUser();
    }

    private boolean isModified() {
        return getDescriptor().isDirectlyCustomizedByUser();
    }

    private boolean isSlotName(String name) {
        return !name.equals(PROPERTY_LAYOUT_PROPERTIES);
    }

    private boolean isSuitable(WidgetDescriptor d, Cls cls, Slot slot) {
        String className = d.getWidgetClassName();
        return className == null || getProject().isSuitableWidget(cls, slot, null, d);
    }

    public void layoutLikeCls(Cls prototype) {
        // Log.enter(this, "layoutLikeCls", prototype);
        PropertyList list = getProject().getClsWidgetPropertyList(prototype);
        setPropertyList((PropertyList) (list.clone()));
        removeAll();
        setModified(true);
        createWidgets();
        revalidate();
        repaint();
    }

    private void layoutWidgets(int fromIndex) {
        // Log.enter(this, "layoutWidgets", new Integer(fromIndex), new Integer(getComponentCount()));
        if (fromIndex < getComponentCount()) {
            _cachedLayoutStrategy.layout(this, fromIndex);
        }
        revalidate();
        repaint();
        postWidgetEvent(WidgetEvent.LAYOUT_CHANGED);
    }

    private void mouseClickedOnWidget(Component c, Point p, int clickCount) {
        if (clickCount == 2) {
            Dimension oldPerferredSize = c.getPreferredSize();
            Widget widget = (Widget) getWidget(c);
            boolean changed = widget.configure();
            if (changed) {
                rebuildWidget(widget);
            }
        }
    }

    private void mouseDrag(JComponent c, Point p) {
        if (_resizeDirection == RESIZE_NONE) {
            dragWidget(c, p);
        } else {
            resizeWidget(c, p);
        }
        updateSize();
    }

    private void mouseDraggedOnForm(Point point) {
        if (_resizeDirection != RESIZE_NONE) {
            resizeWidget(this, getParentPoint(point));
            getParent().invalidate();
            getParent().validate();
            getParent().repaint();
            /*
             * revalidate();
             * repaint();
             */
        }
    }

    public void mouseDraggedOnWidget(Component c, Point p) {
        JComponent widget = getWidget(c);
        Point point = getWidgetPoint(c, p);
        mouseDrag(widget, point);
        _isDragging = true;
        // setCustomized(true);
    }

    private void mouseMove(JComponent c, Point p) {
        setCursor(c, p);
    }

    private void mouseMovedOnForm(Point point) {
        Point p = getParentPoint(point);
        int dir = getResizeDirection(this, p);
        _resizeDirection = dir;
        setCursor(this, dir);
    }

    public void mouseMovedOnWidget(Component c, Point p) {
        JComponent widget = getWidget(c);
        Point point = getWidgetPoint(c, p);
        mouseMove(widget, point);
    }

    private void mousePress(JComponent c, Point p) {
        if (c != _selectedWidget) {
            clearSelection();
            setSelection(c);
        }
        Point topLeft = c.getLocation();
        _cursorOffset = new Dimension(topLeft.x - p.x, topLeft.y - p.y);
        _resizeDirection = getResizeDirection(c, p);
        c.requestFocus();
    }

    private void mousePressedOnForm(Point point) {
        clearSelection();
        repaint();
    }

    private void mousePressedOnWidget(Component c, Point p) {
        JComponent widget = getWidget(c);
        Point point = getWidgetPoint(c, p);
        mousePress(widget, point);
    }

    private void mouseRelease(JComponent c, Point p) {
        _resizeDirection = RESIZE_NONE;
        setCursor(c, p);
    }

    private void mouseReleasedOnWidget(Component c, Point p) {
        JComponent widget = getWidget(c);
        Point point = getWidgetPoint(c, p);
        mouseRelease(widget, point);
        if (_isDragging) {
            _isDragging = false;
            setModified(true);
        }
    }

    public void paint(Graphics g) {
        g.setColor(getBackground());
        g.fillRect(0, 0, getWidth(), getHeight());
        super.paint(g);
    }

    private void propagateSetAssociatedClsToWidgets(Cls associatedCls) {
        for (int i = 0; i < getComponentCount(); ++i) {
            Component c = getComponent(i);
            if (c instanceof Widget) {
                SlotWidget widget = (SlotWidget) c;
                widget.setAssociatedCls(associatedCls);
            }
        }
    }

    private void propagateSetInstanceToWidgets(Instance instance) {
        for (int i = 0; i < getComponentCount(); ++i) {
            Component c = getComponent(i);
            if (c instanceof Widget) {
                SlotWidget widget = (SlotWidget) c;
                widget.setInstance(instance);
            }
        }
    }

    private void rebuildWidget(Widget widget) {
        replaceWidget((Component) widget, widget.getClass().getName());
        setModified(true);
    }

    public void relayout() {
        layoutWidgets(0);
        // HACK HACK need correct event here
        setModified(true);
    }

    public void removeCustomizations() {
        if (canRemoveCustomizations()) {
            getPropertyList().clear();
            setModified(false);
            removeAll();
            initializeWidgets();
            revalidate();
            repaint();
        } else {
            Log.trace("cannot remove customizations for " + getCls(), this, "removeCustomizations");
        }
    }

    private void removeWidget(Slot slot) {
        int startIndex = getCustomizedComponentCount();
        int nComponents = getComponentCount();
        for (int i = 0; i < nComponents; ++i) {
            Component c = getComponent(i);
            if (c instanceof Widget) {
                SlotWidget w = (SlotWidget) c;
                if (w.getSlot() == slot) {
                    if (_selectedWidget == w) {
                        clearSelection();
                    }
                    remove(c);
                    w.dispose();
                    break;
                }
            }
        }
        String name = slot.getName();
        if (name != null) {
            getPropertyList().remove(name);
        }
        layoutWidgets(startIndex);
    }

    public void removeWidgetDescriptor(String slotName) {
        getPropertyList().remove(slotName);
    }

    public void replaceSelectedWidget(String newWidgetClassName) {
        replaceWidget(_selectedWidget, newWidgetClassName);
        setModified(true);
    }

    public void replaceWidget(Slot slot, String newWidgetClassName) {
        // Log.enter(this, "replaceWidget", slot, newWidgetClassName);
        WidgetDescriptor d = getWidgetDescriptor(slot);
        if (d == null) {
            if (newWidgetClassName != null) {
                createWidget(slot, newWidgetClassName);
            }
        } else {
            String oldWidgetClassName = d.getWidgetClassName();
            if (oldWidgetClassName == null) {
                if (newWidgetClassName != null) {
                    int startIndex = getCustomizedComponentCount();
                    d.setWidgetClassName(newWidgetClassName);
                    d.setBounds(new Rectangle());
                    createWidget(d);
                    layoutWidgets(startIndex);
                }
            } else if (!oldWidgetClassName.equals(newWidgetClassName)) {
                replaceWidget(getComponent(slot), newWidgetClassName);
            }
        }
    }

    private void replaceWidget(Component oldComponent, String newClassName) {
        Rectangle bounds = oldComponent.getBounds();
        Dimension oldPreferredSize = oldComponent.getPreferredSize();
        remove(oldComponent);
        SlotWidget oldWidget = (SlotWidget) oldComponent;
        ComponentUtilities.dispose(oldComponent);

        WidgetDescriptor descriptor = oldWidget.getDescriptor();
        descriptor.setWidgetClassName(newClassName);
        descriptor.setBounds(new Rectangle());

        if (newClassName == null) {
            if (_selectedWidget == oldComponent) {
                clearSelection();
                revalidate();
                repaint();
            }
        } else {
            SlotWidget widget = WidgetUtilities.createSlotWidget(descriptor, true, getProject(), getCls(), oldWidget.getSlot());
            setupWidget(widget);
            if (_selectedWidget == oldComponent) {
                setSelection(widget);
            }

            Dimension newPreferredSize = ((JComponent) widget).getPreferredSize();
            if (newPreferredSize.equals(oldPreferredSize) || oldWidget.getClass().getName().equals(newClassName)) {
                ((JComponent) widget).setBounds(bounds);
            } else {
                ((JComponent) widget).setBounds(new Rectangle(bounds.getLocation(), newPreferredSize));
            }
            ((JComponent) widget).revalidate();
        }

    }

    private void resizeWidget(Component widget, Point p) {
        p = gridPoint(p);
        Rectangle r = widget.getBounds();
        switch (_resizeDirection) {
            case RESIZE_N:
                adjustNorth(r, p);
                break;
            case RESIZE_S:
                adjustSouth(r, p);
                break;
            case RESIZE_E:
                adjustEast(r, p);
                break;
            case RESIZE_W:
                adjustWest(r, p);
                break;
            case RESIZE_NW:
                adjustNorth(r, p);
                adjustWest(r, p);
                break;
            case RESIZE_NE:
                adjustNorth(r, p);
                adjustEast(r, p);
                break;
            case RESIZE_SW:
                adjustSouth(r, p);
                adjustWest(r, p);
                break;
            case RESIZE_SE:
                adjustSouth(r, p);
                adjustEast(r, p);
                break;
            default:
                throw new IllegalStateException("Invalid resize direction");
        }
        widget.setBounds(r);
        widget.validate();
        widget.repaint();
    }

    public void setAssociatedCls(Cls associatedCls) {
        // super.setAssociatedCls(associatedCls);
        propagateSetAssociatedClsToWidgets(associatedCls);
    }

    private void setCursor(Component c, int direction) {
        // Log.enter(this, "setCursor", new Integer(direction));
        c.setCursor(Cursor.getPredefinedCursor(direction));
        repaint();
    }

    private void setCursor(JComponent c, Point p) {
        if (c == _selectedWidget) {
            int direction = getResizeDirection(c, p);
            setCursor(c, direction);
        } else {
            setCursor(c, RESIZE_NONE);
        }
    }

    public void setEditable(boolean b) {
        // do nothing
    }

    public void setHorizontalStretcher(Slot slot) {
        setLayoutSlot(PROPERTY_HORIZONTAL_STRETCHER, slot);
    }

    public void setInstance(final Instance instance) {
        super.setInstance(instance);
        propagateSetInstanceToWidgets(instance);
    }

    private void setLayoutSlot(String propertyName, Slot slot) {
        String slotName = (slot == null) ? (String) null : slot.getName();
        getLayoutProperties().setString(propertyName, slotName);
    }

    private void setModified(boolean b) {
        getDescriptor().setDirectlyCustomizedByUser(b);
        getProject().postFormChangeEvent(getDescriptor());
    }

    public void setSelection(Object o) {
        _selectedWidget = (AbstractSlotWidget) o;

        // move to top
        remove(_selectedWidget);
        add(_selectedWidget, 0);

        // set border
        _selectedWidget.setSelectedBorder();
        notifySelectionListeners();
        repaint();
    }

    private void setupWidget(final SlotWidget widget) {
        JComponent component = (JComponent) widget;
        if (isDesignTime()) {
            ComponentUtilities.apply(component, new UnaryFunction() {
                public Object apply(Object o) {
                    Component c = (Component) o;
                    // Disabled components can't set the cursor.  We need to set cursor for resizing
                    // operations. Thus we don't disable the outermost component.
                    if (c != widget) {
                        c.setEnabled(false);
                    }
                    c.addMouseListener(_widgetMouseListener);
                    c.addMouseMotionListener(_widgetMouseMotionListener);
                    c.addKeyListener(_widgetKeyListener);
                    return null;
                }
            });
        } else {
            Slot slot = widget.getSlot();
            if (slot == getHorizontalStretcher()) {
                putClientProperty(ResizingLayout.HORIZONTAL_STRETCHER, component);
            }
            if (slot == getVerticalStretcher()) {
                putClientProperty(ResizingLayout.VERTICAL_STRETCHER, component);
            }
        }
        add(component);
    }

    public void setVerticalStretcher(Slot slot) {
        setLayoutSlot(PROPERTY_VERTICAL_STRETCHER, slot);
    }

    public void setWidgetValues() {
        // do nothing
    }

    private void translate(Component c, int xShift, int yShift) {
        Point location = c.getLocation();
        location.x += xShift * _gridSize;
        location.y += yShift * _gridSize;
        c.setLocation(location);
    }

    // calculate new size based on widgets
    private void updateSize() {

    }

    private void updateWidget(Slot slot) {
        // Log.enter(this, "updateWidget", slot);
        Widget widget = getWidget(slot);
        if (widget != null) {
            if (!getProject().isSuitableWidget(getCls(), slot, null, widget.getDescriptor())) {
                String name = getProject().getDefaultWidgetClassName(getCls(), slot, null);
                replaceWidget((Component) widget, name);
            } else {
                replaceWidget((Component) widget, widget.getClass().getName());
            }
        }
        getProject().postFormChangeEvent(getDescriptor());
    }
}
