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
import java.util.List;
import javax.swing.*;
import javax.swing.table.*;
import edu.stanford.smi.protege.event.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.resource.*;
import edu.stanford.smi.protege.util.*;
import edu.stanford.smi.protege.ui.*;
import edu.stanford.smi.protege.widget.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class TemplateSlotsWidget extends AbstractTableWidget {
    private AllowableAction _viewAction;
    private AllowableAction _viewAtClsAction;
    private AllowableAction _createAction;
    private AllowableAction _addAction;
    private AllowableAction _removeAction;
    private AllowableAction _removeOverrideAction;
    private Collection _currentClsTemplateSlots;

    private ClsListener _clsListener = new ClsAdapter() {
        public void templateSlotAdded(ClsEvent event) {
            Slot slot = event.getSlot();
            slot.addFrameListener(_slotListener);
            _currentClsTemplateSlots.add(slot);
            reload();
        }

        public void templateSlotRemoved(ClsEvent event) {
            Slot slot = event.getSlot();
            slot.removeFrameListener(_slotListener);
            _currentClsTemplateSlots.remove(slot);
            reload();
        }

        public void templateFacetValueChanged(ClsEvent event) {
            repaint();
        }
        public void directSuperclassAdded(ClsEvent event) {
            reload();
        }
        public void directSuperclassRemoved(ClsEvent event) {
            reload();
        }
    };
    private FrameListener _slotListener = new FrameAdapter() {
        public void ownSlotValueChanged(FrameEvent event) {
            super.ownSlotValueChanged(event);
            repaint();
        }
    };

    private KnowledgeBaseListener _knowledgeBaseListener = new KnowledgeBaseAdapter() {
        public void frameNameChanged(KnowledgeBaseEvent event) {
            repaint();
        }
    };

    protected void addInheritedTemplateSlots(Collection slots, Cls cls) {
        Iterator i = cls.getDirectSuperclasses().iterator();
        while (i.hasNext()) {
            Cls superclass = (Cls) i.next();
            addTemplateSlots(slots, superclass);
        }
    }

    private void addSlots(Collection c) {
        Cls cls = getBoundCls();
        Iterator i = c.iterator();
        while (i.hasNext()) {
            cls.addDirectTemplateSlot((Slot) i.next());
        }
    }

    protected void addTemplateSlots(Collection slots, Cls cls) {
        Iterator i = cls.getDirectTemplateSlots().iterator();
        while (i.hasNext()) {
            Slot slot = (Slot) i.next();
            if (!slots.contains(slot)) {
                slots.add(slot);
            }
        }
        addInheritedTemplateSlots(slots, cls);
    }

    private void changeSlotIndex(Slot slot, int delta) {
        List slots = (List) getBoundCls().getDirectTemplateSlots();
        int oldIndex = slots.indexOf(slot);
        int newIndex = oldIndex + delta;
        if (0 <= newIndex && newIndex < slots.size()) {
            Slot templateSlotSlot = getKnowledgeBase().getSlot(Model.Slot.DIRECT_TEMPLATE_SLOTS);
            getBoundCls().moveOwnSlotValue(templateSlotSlot, oldIndex, newIndex);
            reload();
        }
    }

    private Action createMoveDownAction() {
        return new AbstractAction("Move down", Icons.getDownIcon()) {
            {
                putValue(Action.SHORT_DESCRIPTION, "Move selected slot down");
            }
            public void actionPerformed(ActionEvent event) {
                handleMoveDownAction();
            }
        };
    }

    private Action createMoveUpAction() {
        return new AbstractAction("Move up", Icons.getUpIcon()) {
            {
                putValue(Action.SHORT_DESCRIPTION, "Move selected slot up");
            }
            public void actionPerformed(ActionEvent event) {
                handleMoveUpAction();
            }
        };
    }

    public TableModel createTableModel() {
        List slots;
        Cls cls = (Cls) getInstance();
        if (cls == null) {
            slots = Collections.EMPTY_LIST;
        } else {
            slots = getSlots(cls);
        }
        DefaultTableModel model = new DefaultTableModel() {
            public boolean isCellEditable(int row, int col) {
                return false;
            }
        };
        model.addColumn("Name");
        model.addColumn("Type");
        model.addColumn("Cardinality");
        model.addColumn("Other Facets");
        Iterator i = slots.iterator();
        while (i.hasNext()) {
            Slot slot = (Slot) i.next();
            FrameSlotCombination o = new FrameSlotCombination(cls, slot);
            model.addRow(new Object[] { o, o, o, o });
        }
        return model;
    }

    public void dispose() {
        super.dispose();
        getKnowledgeBase().removeKnowledgeBaseListener(_knowledgeBaseListener);
        Cls cls = (Cls) getInstance();
        if (cls != null) {
            cls.removeClsListener(_clsListener);
        }
    }

    public Action getAddSlotsAction() {
        _addAction =
            new AddAction("Attach slots to class") {
                public void onAdd() {
                    List slots = new ArrayList(getKnowledgeBase().getSlots());
                    // slots.removeAll(((Cls) getInstance()).getTemplateSlots());
                    String label = "Select " + getLabel();
                    addSlots(DisplayUtilities.pickSlots(TemplateSlotsWidget.this, slots, label));
                }
            }
        ;
        return _addAction;
    }

    private Cls getBoundCls() {
        return (Cls) getInstance();
    }

    private Action getCreateSlotAction() {
        _createAction = new CreateAction("Create slot and attach to class") {
            public void onCreate() {
                Cls cls = getBoundCls();
                if (cls.isEditable()) {
                    KnowledgeBase kb = getKnowledgeBase();
                    Cls slotMetaCls = kb.getDefaultSlotMetaCls();
                    if (slotMetaCls == null) {
                        Slot templateSlot = getKnowledgeBase().getSlot(Model.Slot.DIRECT_TEMPLATE_SLOTS);
                        Collection allowedClses = cls.getDirectType().getTemplateSlotAllowedClses(templateSlot);
                        slotMetaCls = DisplayUtilities.pickConcreteCls(TemplateSlotsWidget.this, allowedClses);
                    }
                    if (slotMetaCls != null) {
                        Slot slot = getKnowledgeBase().createSlot(null, slotMetaCls);
                        cls.addDirectTemplateSlot(slot);
                        showInstance(slot);
                    }
                }
            }
        };
        return _createAction;
    }

    public Action getDoubleClickAction() {
        return
            new AbstractAction() {
                public void actionPerformed(ActionEvent event) {
                    FrameSlotCombination c = (FrameSlotCombination) CollectionUtilities.getFirstItem(getSelection());
                    if (c != null) {
                        SlotViewPanel panel = new SlotViewPanel();
                        int result = ModalDialog.showDialog(TemplateSlotsWidget.this, panel, "Select Slot View", ModalDialog.MODE_OK_CANCEL);
                        if (result == ModalDialog.OPTION_OK) {
                            if (panel.viewTopLevelSlot()) {
                                _viewAction.actionPerformed(event);
                            } else {
                                _viewAtClsAction.actionPerformed(event);
                            }
                        }
                    }
                }
            }
        ;
    }

    private int getInitialMaxWidth() {
        int tableWidth = getTable().getWidth();
        int viewPortWidth = getWidth() - (getInsets().left + getInsets().right + 3);
        int currentWidth = getTable().getColumnModel().getColumn(4).getWidth();
        int initialWidth = currentWidth + (viewPortWidth - tableWidth);
        return initialWidth;
    }

    public JPopupMenu getPopupMenu() {
        JPopupMenu menu = new JPopupMenu();
        menu.add(_viewAction);
        menu.add(_viewAtClsAction);
        menu.add(_createAction);
        menu.add(_addAction);
        menu.add(_removeAction);
        return menu;
    }

    public Action getRemoveOverrideAction() {
        String text = "Remove overrides from selected slots";
        _removeOverrideAction = new AllowableAction(text, Icons.getRemoveSlotOverrideIcon(), this) {
            public void actionPerformed(ActionEvent event) {
                Iterator i = this.getSelection().iterator();
                while (i.hasNext()) {
                    FrameSlotCombination pair = (FrameSlotCombination) i.next();
                    ((Cls) pair.getFrame()).removeTemplateFacetOverrides(pair.getSlot());
                }
            }
        };
        return _removeOverrideAction;
    }

    public Action getRemoveSlotsAction() {
        _removeAction = new RemoveAction("Remove selected slots from class", this) {
            public void onRemove(Object o) {
                FrameSlotCombination combination = (FrameSlotCombination) o;
                Cls cls = (Cls) combination.getFrame();
                Slot slot = combination.getSlot();
                cls.removeDirectTemplateSlot(slot);
            }

            public void onSelectionChange() {
                Object o = CollectionUtilities.getFirstItem(this.getSelection());
                FrameSlotCombination combination = (FrameSlotCombination) o;
                Slot slot = (combination == null) ? (Slot) null : combination.getSlot();
                setAllowed(slot != null && getBoundCls().hasDirectTemplateSlot(slot));
            }
        };
        return _removeAction;
    }

    private Slot getSelectedDirectSlot() {
        Slot slot = getSelectedSlot();
        boolean isDirect = getBoundCls().hasDirectTemplateSlot(slot);
        return isDirect ? slot : null;
    }

    private Slot getSelectedSlot() {
        Slot slot = null;
        Collection c = getSelection();
        if (c.size() == 1) {
            FrameSlotCombination combo = (FrameSlotCombination) CollectionUtilities.getFirstItem(c);
            slot = combo.getSlot();
        }
        return slot;

    }

    protected List getSlots(Cls cls) {
        List slots = new ArrayList();
        addTemplateSlots(slots, cls);
        return slots;
    }

    private Action getViewSlotAction() {
        _viewAction =
            new ViewAction("View selected slots", this, Icons.getViewTopLevelSlotIcon()) {
                public void onView(Object o) {
                    FrameSlotCombination combination = (FrameSlotCombination) o;
                    showInstance(combination.getSlot());
                }
            }
        ;
        return _viewAction;
    }

    private Action getViewSlotAtClassAction() {
        _viewAtClsAction =
            new ViewAction("View selected slots at class", this, Icons.getViewSlotAtClassIcon()) {
                public void onView(Object o) {
                    FrameSlotCombination combination = (FrameSlotCombination) o;
                    show((Cls) combination.getFrame(), combination.getSlot());
                }
            }
        ;
        return _viewAtClsAction;
    }

    protected void handleMoveDownAction() {
        Slot slot = getSelectedDirectSlot();
        if (slot != null) {
            changeSlotIndex(slot, +1);
        }
    }

    protected void handleMoveUpAction() {
        Slot slot = getSelectedDirectSlot();
        if (slot != null) {
            changeSlotIndex(slot, -1);
        }
    }

    public void initialize() {
        Action viewSlotAction = getViewSlotAction();
        super.initialize(getDoubleClickAction());

        getTable().setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
        addButton(viewSlotAction);
        addButton(getViewSlotAtClassAction());
        addButton(getCreateSlotAction());
        addButton(getRemoveOverrideAction());
        addButton(getAddSlotsAction());
        addButton(getRemoveSlotsAction());
        addButton(createMoveUpAction(), false);
        addButton(createMoveDownAction(), false);

        addColumn(200, SlotPairRenderer.createInstance());
        addColumn(60, new TypeFacetRenderer());
        addColumn(100, new CardinalityFacetRenderer(getKnowledgeBase()));
        addColumn(200, new OtherFacetsRenderer());

        getKnowledgeBase().addKnowledgeBaseListener(_knowledgeBaseListener);
    }

    public static boolean isSuitable(Cls cls, Slot slot, Facet facet) {
        return slot.getName().equals(Model.Slot.DIRECT_TEMPLATE_SLOTS);
    }

    public void reload() {
        super.reload();
        setOtherFacetsWidth();
    }

    public void reshape(int x, int y, int w, int h) {
        super.reshape(x, y, w, h);
        setOtherFacetsWidth();
    }

    public void setEditable(boolean b) {
        _createAction.setAllowed(b);
        _addAction.setAllowed(b);
        _removeAction.setAllowed(b);
        _removeOverrideAction.setAllowed(b);
    }

    public void setInstance(Instance instance) {
        Cls currentCls = (Cls) getInstance();
        if (currentCls != null) {
            currentCls.removeClsListener(_clsListener);
            Iterator i = _currentClsTemplateSlots.iterator();
            while (i.hasNext()) {
                Slot slot = (Slot) i.next();
                slot.removeFrameListener(this._slotListener);
            }
        }
        super.setInstance(instance);
        if (instance != null) {
            Cls cls = (Cls) instance;
            cls.addClsListener(_clsListener);
            _currentClsTemplateSlots = new ArrayList(cls.getTemplateSlots());
            Iterator i = _currentClsTemplateSlots.iterator();
            while (i.hasNext()) {
                Slot slot = (Slot) i.next();
                slot.addFrameListener(this._slotListener);
            }
        }
    }

    private void setOtherFacetsWidth() {
        JTable table = getTable();
        if (table != null && table.getColumnCount() == 5 && table.getWidth() > 0) {
            TableColumn column = table.getColumnModel().getColumn(4);
            TableCellRenderer renderer = column.getCellRenderer();
            int maxWidth = getInitialMaxWidth();
            int col = 4;
            int nRows = table.getRowCount();
            for (int row = 0; row < nRows; ++row) {
                FrameSlotCombination c = (FrameSlotCombination) table.getValueAt(row, col);
                Component component = renderer.getTableCellRendererComponent(table, c, false, false, row, col);
                Dimension d = component.getPreferredSize();
                maxWidth = Math.max(maxWidth, d.width);
            }
            column.setPreferredWidth(maxWidth);
        }
    }

    public String toString() {
        return "TemplateSlotsWidget";
    }

    public void validateTree() {
        super.validateTree();
        setOtherFacetsWidth();
    }
}
