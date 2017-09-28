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

package edu.stanford.smi.protege.util;


import java.awt.*;
import java.awt.event.*;
import java.net.*;
import java.util.*;
import java.util.List;
import javax.swing.*;
import javax.swing.table.*;
import javax.swing.tree.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class ComponentUtilities {
    private static final int BORDER_SIZE = 50;
    private static final int STANDARD_ROW_HEIGHT = 60;
    private static final int STANDARD_COLUMN_WIDTH = 125;
    private static Collection _openWindows = new ArrayList();

    public static void addColumn(JTable table, TableCellRenderer renderer) {
        addColumn(table, renderer, null);
    }

    public static void addColumn(JTable table, TableCellRenderer renderer, TableCellEditor editor) {
        int nColumns = table.getColumnCount();
        TableColumn column = new TableColumn(nColumns);
        column.setCellRenderer(renderer);
        column.setCellEditor(editor);
        table.addColumn(column);
    }

    public static int addListValue(JList list, Object newValue) {
        return getModel(list).addValue(newValue);
    }

    public static void addListValue(JList list, Object newValue, int index) {
        getModel(list).addValue(newValue, index);
    }

    public static int addListValue(JList list, Object newValue, Comparator comparator) {
        int index = getPositionIndex(list, newValue, comparator);
        addListValue(list, newValue, index);
        return index;
    }

    public static void addListValues(JList list, Collection newValues) {
        if (!newValues.isEmpty()) {
            getModel(list).addValues(newValues);
        }
    }

    public static int addSelectedListValue(JList list, Object newValue) {
        int index = getModel(list).addValue(newValue);
        list.setSelectedIndex(index);
        return index;
    }

    public static void addSelectedListValues(JList list, Collection newValues) {
        if (!newValues.isEmpty()) {
            int index = getModel(list).addValues(newValues);
            list.setSelectionInterval(index, index + newValues.size() - 1);
        }
    }

    public static void addUniqueListValues(JList list, Collection newValues) {
        Collection uniqueValues = new HashSet(newValues);
        uniqueValues.removeAll(getModel(list).getValues());
        addListValues(list, uniqueValues);
    }

    public static void apply(Component component, UnaryFunction f) {
        f.apply(component);
        applyToDescendents(component, f);
    }

    public static void applyToDescendents(Component component, UnaryFunction f) {
        if (component instanceof Container) {
            Container container = (Container) component;
            int count = container.getComponentCount();
            for (int i = 0; i < count; ++i) {
                Component subComponent = container.getComponent(i);
                apply(subComponent, f);
            }
        }
    }

    public static void center(Component c) {
        Dimension screenSize = c.getToolkit().getScreenSize();
        screenSize.width -= BORDER_SIZE;
        screenSize.height -= BORDER_SIZE;
        Dimension componentSize = c.getSize();
        int xPos = (screenSize.width - componentSize.width) / 2;
        xPos = Math.max(xPos, 0);
        int yPos = (screenSize.height - componentSize.height) / 2;
        yPos = Math.max(yPos, 0);
        c.setLocation(new Point(xPos, yPos));
    }

    public static void clearListValues(JList list) {
        getModel(list).clear();
    }

    private static void clearSelectionIfNecessary(JList list, int count) {
        // Workaround for swing bug.  Removing all elements from a list does not cause a selection event
        // to get fired.  setSelectedIndex(-1) also does not cause an event to fire.  Thus we clear the
        // selection manually if the result of the remove is that the list will be empty
        if (list.getModel().getSize() == count) {
            list.clearSelection();
        }
    }

    public static void closeAllWindows() {
        Iterator i = new ArrayList(_openWindows).iterator();
        while (i.hasNext()) {
            Window w = (Window) i.next();
            closeWindow(w);
        }
    }

    public static void closeWindow(Window window) {
        window.dispatchEvent(new WindowEvent(window, WindowEvent.WINDOW_CLOSING));
    }

    public static void deregisterWindow(Window window) {
        _openWindows.remove(window);
    }

    private static void disassemble(Component component) {
        if (component instanceof Container) {
            Container container = (Container) component;
            int nSubcomponents = container.getComponentCount();
            for (int i = 0; i < nSubcomponents; ++i) {
                disassemble(container.getComponent(i));
            }
            container.removeAll();
        }
    }

    public static void dispose(Component component) {
        component.setVisible(false);
        UnaryFunction dispose = new UnaryFunction() {
            public Object apply(Object o) {
                if (o instanceof Disposable) {
                    ((Disposable) o).dispose();
                    // Log.enter("ComponentUtilities.dispose", o.getClass().getName());
                }
                return Boolean.TRUE;
            }
        };
        apply(component, dispose);
        disassemble(component);
    }

    public static void ensureSelectionIsVisible(final JList list) {
        /*
         * SwingUtilities.invokeLater(new Runnable() {
         * public void run() {
         * int selection = list.getSelectedIndex();
         * list.ensureIndexIsVisible(selection);
         * }
         * });
         */
    }

    public static void extendSelection(JTree tree, Object userObject) {
        LazyTreeNode selectedNode = (LazyTreeNode) tree.getLastSelectedPathComponent();
        int index = selectedNode.getUserObjectIndex(userObject);
        TreeNode node = selectedNode.getChildAt(index);
        setSelectedNode(tree, node);
    }

    private static int fullExpand(JTree tree, TreePath parentPath, int nExpansions) {
        TreeNode parent = (TreeNode) parentPath.getLastPathComponent();
        int count = parent.getChildCount();
        for (int i = 0; i < count && nExpansions > 0; ++i) {
            TreeNode child = parent.getChildAt(i);
            TreePath childPath = parentPath.pathByAddingChild(child);
            nExpansions = fullExpand(tree, childPath, nExpansions);
        }
        tree.expandPath(parentPath);
        return --nExpansions;
    }

    public static void fullSelectionCollapse(JTree tree) {
        int startRow = tree.getLeadSelectionRow();
        int stopRow = getStopRow(tree, startRow);
        for (int i = stopRow - 1; i >= startRow; --i) {
            tree.collapseRow(i);
        }
    }

    public static void fullSelectionExpand(JTree tree, int max_expansions) {
        TreePath topPath = tree.getLeadSelectionPath();
        fullExpand(tree, topPath, max_expansions);
    }

    public static LazyTreeNode getChildNode(LazyTreeNode node, Object userObject) {
        LazyTreeNode childNode = null;
        int nChildren = node.getChildCount();
        for (int i = 0; i < nChildren; ++i) {
            childNode = (LazyTreeNode) node.getChildAt(i);
            if (childNode.getUserObject() == userObject) {
                break;
            }
        }
        return childNode;
    }

    public static Component getDescendentOfClass(Class componentClass, Component root) {
        Collection c = getDescendentsOfClass(componentClass, root);
        Assert.assertTrue("max 1 descendent", c.size() == 0 || c.size() == 1);
        return (Component) CollectionUtilities.getFirstItem(c);
    }

    public static Collection getDescendentsOfClass(final Class componentClass, Component root) {
        final Collection results = new ArrayList();
        UnaryFunction f = new UnaryFunction() {
            public Object result = null;
            public Object apply(Object o) {
                if (componentClass.isInstance(o)) {
                    results.add(o);
                }
                return null;
            }
        };
        apply(root, f);
        return results;
    }

    public static Dialog getDialog(Component c) {
        return (Dialog) SwingUtilities.windowForComponent(c);
    }

    public static Object getFirstSelectionParent(JTree tree) {
        Object parent;
        LazyTreeNode node = (LazyTreeNode) tree.getLastSelectedPathComponent();
        if (node == null) {
            parent = null;
        } else {
            LazyTreeNode parentNode = node.getLazyTreeNodeParent();
            if (parentNode instanceof LazyTreeRoot) {
                parent = null;
            } else {
                parent = parentNode.getUserObject();
            }
        }
        return parent;
    }

    public static Frame getFrame(Component c) {
        Frame frame;
        if (c instanceof Frame) {
            frame = (Frame) c;
        } else {
            frame = (Frame) SwingUtilities.windowForComponent(c);
        }
        return frame;
    }

    public static Collection getListValues(JList list) {
        return getModel(list).getValues();
    }

    private static SimpleListModel getModel(JList list) {
        ListModel model = list.getModel();
        if (!(model instanceof SimpleListModel)) {
            model = new SimpleListModel();
            list.setModel(model);
        }
        return (SimpleListModel) model;
    }

    private static int getPositionIndex(JList list, Object value, Comparator comparator) {
        int index = Collections.binarySearch(getModel(list).getValues(), value, comparator);
        if (index < 0) {
            index = -(index + 1);
        }
        return index;
    }

    public static Object getSelectedValue(JList list) {
        return CollectionUtilities.getFirstItem(getSelection(list));
    }

    public static Collection getSelection(JList list) {
        return Arrays.asList(list.getSelectedValues());
    }

    public static Collection getSelection(JTable table) {
        TableModel model = table.getModel();
        int[] indices = table.getSelectedRows();
        Collection selection = new ArrayList();
        for (int i = 0; i < indices.length; ++i) {
            selection.add(model.getValueAt(indices[i], 0));
        }
        return selection;
    }

    public static Collection getSelection(JTree tree) {
        return getSelection(tree, Object.class);
    }

    public static Collection getSelection(JTree tree, Class c) {
        Assert.assertNotNull("tree", tree);
        Collection selections = new HashSet();
        TreePath[] paths = tree.getSelectionModel().getSelectionPaths();
        if (paths != null) {
            for (int i = 0; i < paths.length; ++i) {
                Object o = paths[i].getLastPathComponent();
                if (o instanceof LazyTreeNode) {
                    o = ((LazyTreeNode) o).getUserObject();
                }
                if (c == null || c.isInstance(o)) {
                    selections.add(o);
                }
            }
        }
        return selections;
    }

    public static int getStandardColumnWidth() {
        return STANDARD_COLUMN_WIDTH;
    }

    public static int getStandardRowHeight() {
        return STANDARD_ROW_HEIGHT;
    }

    private static int getStopRow(JTree tree, int startRow) {
        int startDepth = tree.getPathForRow(startRow).getPathCount();
        int last = tree.getRowCount();
        int stopRow = last;
        for (int i = startRow + 1; i < last; ++i) {
            int depth = tree.getPathForRow(i).getPathCount();
            if (depth <= startDepth) {
                stopRow = i;
                break;
            }
        }
        return stopRow;
    }

    public static TreePath getTreePath(JTree tree, Collection objectPath) {
        List nodePath = new LinkedList();
        LazyTreeNode node = (LazyTreeNode) tree.getModel().getRoot();
        nodePath.add(node);
        Iterator i = objectPath.iterator();
        while (i.hasNext()) {
            Object userObject = i.next();
            node = getChildNode(node, userObject);
            if (node == null) {
                Log.warning("no node for " + userObject, ComponentUtilities.class, "getTreePath", objectPath);
                break;
            }
            nodePath.add(node);
        }
        return new TreePath(nodePath.toArray());
    }

    public static void hide(final Component c, final int delayInMillisec) {
        Thread t = new Thread() {
            public void run() {
                try {
                    sleep(delayInMillisec);
                    Component topComponent = SwingUtilities.getRoot(c);
                    topComponent.setVisible(false);
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        };
        t.start();
    }

    public static boolean isDragAndDropEnabled(JComponent c) {
        Object o = c.getClientProperty(ComponentUtilities.class);
        return (o == null) ? true : ((Boolean) o).booleanValue();
    }

    public static boolean listValuesContain(JList list, Object value) {
        return getModel(list).contains(value);
    }

    public static ImageIcon loadImageIcon(Class cls, String name) {
        ImageIcon icon = null;
        URL url = cls.getResource(name);
        if (url != null) {
            icon = new ImageIcon(url);
            if (icon.getIconWidth() == -1) {
                Log.error("failed to load", SystemUtilities.class, "loadImageIcon", cls, name);
            }
        }
        return icon;
    }

    // HACK: work around JDK1.2 swing layout bug by calling pack twice.
    public static void pack(Component c) {
        Window window = getFrame(c);
        window.pack();
        window.pack();
    }

    public static void paintImmediately(Component component) {
        Graphics g = component.getGraphics();
        component.paint(g);
        g.dispose();
    }

    public static void pressButton(Component c, final Icon icon) {
        Iterator i = getDescendentsOfClass(JButton.class, c).iterator();
        while (i.hasNext()) {
            JButton button = (JButton) i.next();
            if (button.getIcon() == icon) {
                button.doClick();
                // Log.trace("doClick", ComponentUtilities.class, "pressButton", c, icon);
            }
        }
    }

    public static void registerWindow(Window window) {
        _openWindows.add(window);
    }

    public static void removeListValue(JList list, Object oldValue) {
        clearSelectionIfNecessary(list, 1);
        int selectedIndex = list.getSelectedIndex();
        int index = getModel(list).removeValue(oldValue);
        if (selectedIndex == index) {
            setSelectedIndex(list, index);
        }
    }

    public static void removeListValues(JList list, Collection values) {
        clearSelectionIfNecessary(list, values.size());
        int selectedIndex = list.getSelectedIndex();
        int index = getModel(list).removeValues(values);
        if (selectedIndex == index) {
            setSelectedIndex(list, index);
        }
    }

    public static void removeSelection(JTree tree) {
        LazyTreeNode selectedNode = (LazyTreeNode) tree.getLastSelectedPathComponent();
        LazyTreeNode parentNode = selectedNode.getLazyTreeNodeParent();
        int index = parentNode.getUserObjectIndex(selectedNode.getUserObject());
        int nChildren = parentNode.getChildCount();
        TreeNode newSelection;
        if (index == nChildren - 1) {
            if (nChildren == 1) {
                newSelection = parentNode;
            } else {
                newSelection = parentNode.getChildAt(index - 1);
            }
        } else {
            newSelection = parentNode.getChildAt(index + 1);
        }
        setSelectedNode(tree, newSelection);
    }

    public static void replaceListValue(JList list, Object oldValue, Object newValue) {
        SimpleListModel model = getModel(list);
        int index = model.indexOf(oldValue);
        model.setValue(index, newValue);
    }

    public static void reposition(JList list, Object value, Comparator comparator) {
        int oldSelectionIndex = list.getSelectedIndex();

        SimpleListModel model = getModel(list);
        int fromIndex = model.indexOf(value);
        model.removeValue(value);

        int toIndex = getPositionIndex(list, value, comparator);
        getModel(list).addValue(value, toIndex);

        if (oldSelectionIndex != -1) {
            int newSelectionIndex = oldSelectionIndex;
            if (fromIndex == oldSelectionIndex) {
                newSelectionIndex = toIndex;
            } else if (fromIndex < oldSelectionIndex && toIndex > oldSelectionIndex) {
                --newSelectionIndex;
            } else if (fromIndex > oldSelectionIndex && toIndex < oldSelectionIndex) {
                ++newSelectionIndex;
            }
            list.setSelectedIndex(newSelectionIndex);
            list.ensureIndexIsVisible(newSelectionIndex);
        }
    }

    public static void setDragAndDropEnabled(JComponent c, boolean enable) {
        c.putClientProperty(ComponentUtilities.class, new Boolean(enable));
    }

    public static void setEnabled(Component component, final boolean enabled) {
        apply(component,
            new UnaryFunction() {
                public Object apply(Object o) {
                    ((Component) o).setEnabled(enabled);
                    return null;
                }
            }
        );
    }

    public static void setExpanded(JTree tree, Collection objectPath, boolean expand) {
        TreePath path = getTreePath(tree, objectPath);
        if (expand) {
            tree.scrollPathToVisible(path);
            tree.expandPath(path);
        } else {
            tree.collapsePath(path);
        }
    }

    public static void setFrameTitle(Component c, String title) {
        getFrame(c).setTitle(title);
    }

    public static void setListValues(JList list, Collection values) {
        getModel(list).setValues(values);
    }

    private static void setSelectedIndex(JList list, int index) {
        int nElements = list.getModel().getSize();
        index = Math.min(index, nElements - 1);
        list.setSelectedIndex(index);
    }

    public static void setSelectedNode(JTree tree, TreeNode node) {
        final TreePath path = new TreePath(((LazyTreeModel) tree.getModel()).getPathToRoot(node));
        tree.scrollPathToVisible(path);
        tree.setSelectionPath(path);
    }

    public static void setSelectedObjectPath(JTree tree, Collection objectPath) {
        TreePath path = getTreePath(tree, objectPath);
        tree.scrollPathToVisible(path);
        tree.setSelectionPath(path);
    }
}
