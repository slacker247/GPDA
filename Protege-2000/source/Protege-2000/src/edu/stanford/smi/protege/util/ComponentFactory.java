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
import java.awt.datatransfer.*;
import java.awt.dnd.*;
import java.awt.event.*;
import java.beans.*;
import java.io.*;
import java.util.*;
import javax.swing.*;
import javax.swing.border.*;
import edu.stanford.smi.protege.resource.*;

/**
 * Factory class for making swing components, and their varients.  The use of this
 * class is not required for Protege widgets.  It is encouraged though.  This allows
 * for a single place to address swing bugs and look and feel issues.
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class ComponentFactory {
    public final static int STANDARD_BUTTON_HEIGHT = 25;
    public final static int STANDARD_BUTTON_WIDTH = 25;
    public final static int STANDARD_FIELD_HEIGHT = STANDARD_BUTTON_HEIGHT;
    public final static Dimension STANDARD_BUTTON_SIZE = new Dimension(STANDARD_BUTTON_WIDTH, STANDARD_BUTTON_HEIGHT);
    public final static int SMALL_BUTTON_HEIGHT = 16;
    public final static int SMALL_BUTTON_WIDTH = 16;
    public final static Dimension SMALL_BUTTON_SIZE = new Dimension(SMALL_BUTTON_WIDTH, SMALL_BUTTON_HEIGHT);
    private static String _lastDirectory;

    private static int _offset;
    private final static int OFFSET_SIZE = 25;

    private static class DisposableFrame extends JFrame implements Disposable {
        public DisposableFrame() {
            ComponentUtilities.registerWindow(this);
            enableEvents(AWTEvent.WINDOW_EVENT_MASK);
        }

        public void processWindowEvent(WindowEvent event) {
            if (event.getID() == WindowEvent.WINDOW_CLOSED) {
                ComponentUtilities.deregisterWindow(this);
            }
            super.processWindowEvent(event);
        }

        public void finalize() {
            try {
                super.finalize();
                // System.out.println(getClass().getName() + " finalize");
            } catch (Throwable t) {
                t.printStackTrace();
            }
        }
    }

    public static void addMenuItem(JMenu menu, Action action) {
        Object icon = action.getValue(Action.SMALL_ICON);
        JMenuItem item = menu.add(action);
        if (item.getIcon() == null) {
            item.setIcon(Icons.getBlankIcon());
        }
        item.setDisabledIcon((Icon) action.getValue(Action.SMALL_ICON));
    }

    public static void addMenuItemNoIcon(JMenu menu, Action action) {
        JMenuItem item = menu.add(action);
    }

    public static void addSubmenu(JMenu menu, JMenu submenu) {
        submenu.setIcon(Icons.getBlankIcon());
        submenu.setHorizontalTextPosition(JMenu.RIGHT);
        menu.add(submenu);
    }

    public static JToggleButton addToggleToolBarButton(FakeToolBar toolBar, final Action action) {
        JToggleButton button = new JToggleButton((Icon) action.getValue(Action.SMALL_ICON));
        addToolBarButton(toolBar, action, button);
        return button;
    }

    public static JButton addToolBarButton(FakeToolBar toolBar, final Action action) {
        JButton button = new JButton((Icon) action.getValue(Action.SMALL_ICON));
        addToolBarButton(toolBar, action, button);
        return button;
    }

    public static void addToolBarButton(FakeToolBar toolBar, final Action action, final AbstractButton button) {
        button.addActionListener(action);
        String toolTip = (String) action.getValue(Action.SHORT_DESCRIPTION);
        if (toolTip == null) {
            toolTip = (String) action.getValue(Action.NAME);
        }
        button.setToolTipText(toolTip);
        button.setFocusPainted(false);

        action.addPropertyChangeListener(
            new PropertyChangeListener() {
                public void propertyChange(PropertyChangeEvent event) {
                    button.setEnabled(action.isEnabled());
                }
            }
        );
        button.setEnabled(action.isEnabled());

        toolBar.addButton(button);

        // This screwy code tries to work around the Motif LAF button border display problem
        Border border = BorderFactory.createCompoundBorder(button.getBorder(), null);
        button.setBorder(border);
    }

    public static JButton addToolBarButton(JToolBar bar, Action action) {
        JButton button = bar.add(action);
        button.setToolTipText((String) action.getValue(Action.SHORT_DESCRIPTION));
        button.setText("");
        button.setFocusPainted(false);

        button.setPreferredSize(STANDARD_BUTTON_SIZE);
        button.setMaximumSize(STANDARD_BUTTON_SIZE);
        button.setMinimumSize(STANDARD_BUTTON_SIZE);
        button.setSize(STANDARD_BUTTON_SIZE);

        // This screwy code trys to work around the Motif LAF button border display problem
        // Border border = BorderFactory.createCompoundBorder(button.getBorder(), null);
        // button.setBorder(border);
        return button;
    }

    private static void adjustPosition(Component c) {
        _offset = (_offset + 1) % 4;
        Point p = c.getLocation();
        p.x += _offset * OFFSET_SIZE;
        p.y += _offset * OFFSET_SIZE;
        c.setLocation(p);
    }

    private static Dimension buttonPreferredHeightSize(Dimension d) {
        d.height = STANDARD_BUTTON_HEIGHT;
        return d;
    }

    private static void configureList(JList list, Action action, boolean enableDragAndDrop) {
        list.setModel(new SimpleListModel());
        if (action != null) {
            list.addMouseListener(new DoubleClickActionAdapter(action));
        }
        if (enableDragAndDrop) {
            setupDragAndDrop(list);
            ComponentUtilities.setDragAndDropEnabled(list, enableDragAndDrop);
        }
        list.setCellRenderer(new DefaultRenderer());
    }

    public static void configureTree(JTree tree, Action action) {
        if (action != null) {
            tree.addMouseListener(new DoubleClickActionAdapter(action));
        }

        // this causes reasonable looking parent to child lines to be drawn
        tree.putClientProperty("JTree.lineStyle", "Angled");
        tree.setRootVisible(false);
        tree.setShowsRootHandles(false);
    }

    public static JButton createButton(Action action) {
        JButton button =
            new JButton() {
                public Dimension getPreferredSize() {
                    return buttonPreferredHeightSize(super.getPreferredSize());
                }
            }
        ;
        initializeAbstractButton(button, action);
        return button;
    }

    public static JPanel createButtonPreferredHeightPanel() {
        JPanel panel = new JPanel() {
            public Dimension getPreferredSize() {
                return buttonPreferredHeightSize(super.getPreferredSize());
            }
        };
        return panel;
    }

    public static JCheckBox createCheckBox() {
        return createCheckBox("");
    }

    public static JCheckBox createCheckBox(String s) {
        JCheckBox checkBox = new JCheckBox(s);
        return checkBox;
    }

    public static JComboBox createComboBox() {
        JComboBox comboBox =
            new JComboBox() {
                public Dimension getPreferredSize() {
                    return fieldPreferredHeightSize(super.getPreferredSize());
                }
            }
        ;
        return comboBox;
    }

    public static FakeToolBar createFakeToolBar() {
        return new FakeToolBar(SwingConstants.LEFT);
    }

    public static FakeToolBar createFakeToolBar(Dimension d) {
        return new FakeToolBar(SwingConstants.LEFT, d);
    }

    public static JFileChooser createFileChooser(String description, String extension) {
        if (_lastDirectory == null) {
            _lastDirectory = FileUtilities.getCurrentWorkingDirectory();
            if (_lastDirectory == null) {
                _lastDirectory = ApplicationProperties.getApplicationDirectory();
            }
        }

        JFileChooser chooser = new JFileChooser(_lastDirectory) {
            public int showDialog(Component c, String s) {
                int rval = super.showDialog(c, s);
                _lastDirectory = getCurrentDirectory().getPath();
                return rval;
            }
        };
        chooser.setDialogTitle(description);
        if (extension == null) {
            chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
        } else {
            String text = Text.getProgramName() + " " + description;
            chooser.setFileFilter(new ExtensionFilter(extension, text));
        }
        return chooser;
    }

    public static JFrame createFrame() {
        JFrame frame = new DisposableFrame();
        frame.setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
        Image nerd = Icons.getNerd16x16Image();
        frame.setIconImage(nerd);
        return frame;
    }

    public static JLabel createLabel() {
        JLabel label = new JLabel();
        return label;
    }

    public static JSplitPane createLeftRightSplitPane() {
        return createLeftRightSplitPane(true);
    }

    public static JSplitPane createLeftRightSplitPane(boolean autoResize) {
        return createSplitPane(JSplitPane.HORIZONTAL_SPLIT, autoResize);
    }

    public static JList createList(Action action) {
        return createList(action, false);
    }

    public static JList createList(Action action, boolean enableDragAndDrop) {
        return createSelectableList(action, enableDragAndDrop);
    }

    public static JFrame createMainFrame() {
        JFrame frame = new JFrame();
        Image nerd = Icons.getNerd16x16Image();
        frame.setIconImage(nerd);
        return frame;
    }

    public static JMenu createMenu() {
        JMenu menu = new JMenu();
        return menu;
    }

    /*
     * public static JLabel createLabelBox() {
     * JLabel label = new JLabel() {
     * public Dimension getPreferredSize() {
     * return preferredHeightSize(super.getPreferredSize());
     * }
     * };
     * label.setFont(null);
     * label.setForeground(null);
     * label.setBorder(BorderFactory.createEtchedBorder());
     * return label;
     * }
     */

    public static JPanel createPanel() {
        JPanel panel = new JPanel();
        return panel;
    }

    public static JPasswordField createPasswordField() {
        JPasswordField passwordField = new JPasswordField() {
            public Dimension getPreferredSize() {
                return fieldPreferredHeightSize(super.getPreferredSize());
            }
        };
        return passwordField;
    }

    public static JRadioButtonMenuItem createRadioButtonMenuItem(Action action) {
        JRadioButtonMenuItem item = new JRadioButtonMenuItem();
        initializeAbstractButton(item, action);
        return item;
    }

    public static JScrollPane createScrollPane(JComponent c) {
        JScrollPane pane = new JScrollPane(c);
        return pane;
    }

    public static JScrollPane createScrollPane(JTable table) {
        JScrollPane pane =
            new JScrollPane(table) {
                // This works in JDK 1.2 (but not 1.3)
                // This makes background color to be the same as the table background color.  There should
                // be a better way to do this than subclassing but none of the obvious ways work.  Tables don't
                // stretch to fill up the pane like list boxes do.
                public void paint(Graphics g) {
                    g.setColor(getViewport().getView().getBackground());
                    g.fillRect(0, 0, getWidth(), getHeight());
                    super.paint(g);
                }
            }
        ;
        // This works in JDK 1.3 (but not 1.2)
        JViewport viewPort = pane.getViewport();
        viewPort.setBackground(table.getBackground());

        table.setDoubleBuffered(false);
        return pane;
    }

    public static SelectableList createSelectableList(Action action) {
        return createSelectableList(action, false);
    }

    public static SelectableList createSelectableList(Action action, boolean enableDragAndDrop) {
        SelectableList list = new SelectableList();
        configureList(list, action, enableDragAndDrop);
        return list;
    }

    public static SelectableTable createSelectableTable(Action action) {
        SelectableTable table = new SelectableTable();
        if (action != null) {
            table.addMouseListener(new DoubleClickActionAdapter(action));
        }
        table.setShowGrid(false);
        table.setIntercellSpacing(new Dimension(0, 0));
        table.setColumnSelectionAllowed(false);
        table.setAutoResizeMode(JTable.AUTO_RESIZE_LAST_COLUMN);
        table.setAutoCreateColumnsFromModel(false);
        table.setDefaultEditor(Object.class, null);
        return table;
    }

    public static SelectableTree createSelectableTree(Action action) {
        return createSelectableTree(action, null);
    }

    public static SelectableTree createSelectableTree(Action action, LazyTreeRoot root) {
        SelectableTree tree = new SelectableTree(action, root);
        return tree;
    }

    public static SelectableList createSingleItemList(Action action) {
        SelectableList list = (SelectableList) createList(action);
        list.setPreferredSize(new Dimension(1, 24));
        list.setFixedCellHeight(20);
        list.setBorder(BorderFactory.createEtchedBorder());
        return list;
    }

    // implemementation
    private static JSplitPane createSplitPane(int direction, final boolean autoResize) {
        JSplitPane pane =
            new JSplitPane(direction, true) {
                public void addImpl(Component c, Object o, int i) {
                    if (c instanceof JComponent) {
                        // ((JComponent)c).setMinimumSize(SMALL_SIZE);
                        ((JComponent) c).setMinimumSize(new Dimension(0, 0));
                    }
                    super.addImpl(c, o, i);
                }

                public void updateUI() {
                    super.updateUI();
                    setBorder(null);
                }

                public String toString() {
                    return "SplitPane";
                }
            }
        ;
        pane.setOneTouchExpandable(true);
        return pane;
    }

    public static Border createStandardBorder() {
        return BorderFactory.createEmptyBorder(5, 5, 5, 5);
    }

    public static JTabbedPane createTabbedPane(final boolean addBorder) {
        JTabbedPane pane =
            new JTabbedPane() {
                public void addImpl(Component component, Object constraints, int index) {
                    if (addBorder) {
                        JComponent c = (JComponent) component;
                        c.setBorder(BorderFactory.createCompoundBorder(createStandardBorder(), c.getBorder()));
                    }
                    super.addImpl(component, constraints, index);
                }

                // This mysterious override is a workaround for a drag and drop but in JDK1.2.  Without this code,
                // drop targets on tabs other than the first one never get any notifications.
                public Component findComponentAt(int x, int y) {
                    if (!contains(x, y)) {
                        return null;
                    }
                    int ncomponents = getComponentCount();
                    for (int i = 0; i < ncomponents; i++) {
                        Component comp = getComponentAt(i);
                        if (comp != null) {
                            if (comp instanceof Container) {
                                if (comp.isVisible()) {
                                    comp = ((Container) comp).findComponentAt(x - comp.getX(), y - comp.getY());
                                }
                            } else {
                                comp = comp.getComponentAt(x - comp.getX(), y - comp.getY());
                            }
                            if (comp != null && comp.isVisible()) {
                                return comp;
                            }
                        }
                    }
                    return this;
                }
            }
        ;
        return pane;
    }

    public static JTable createTable(Action action) {
        return createSelectableTable(action);
    }

    public static JTextArea createTextArea() {
        JTextArea area =
            new JTextArea() {
                public void setText(String text) {
                    super.setText(text);
                    setCaretPosition(0);
                    repaint();
                }

                public void paste() {
                    super.paste();
                    repaint();
                }
            }
        ;
        area.setLineWrap(true);
        area.setWrapStyleWord(true);
        return area;
    }

    public static JTextField createTextField() {
        JTextField textField = new JTextField() {
            public Dimension getPreferredSize() {
                return fieldPreferredHeightSize(super.getPreferredSize());
            }
        };
        return textField;
    }

    public static JTextPane createTextPane() {
        JTextPane pane = new JTextPane();
        return pane;
    }

    public static JToggleButton createToggleButton(Action action) {
        JToggleButton button =
            new JToggleButton() {
                public Dimension getPreferredSize() {
                    return buttonPreferredHeightSize(super.getPreferredSize());
                }
            }
        ;
        initializeAbstractButton(button, action);
        return button;
    }

    public static JToolBar createToolBar() {
        JToolBar bar =
            new JToolBar() {
                public void updateUI() {
                    super.updateUI();
                    setBorder(null);
                }
            }
        ;
        bar.setFloatable(false);
        return bar;
    }

    public static JSplitPane createTopBottomSplitPane() {
        return createTopBottomSplitPane(true);
    }

    public static JSplitPane createTopBottomSplitPane(boolean autoResize) {
        return createSplitPane(JSplitPane.VERTICAL_SPLIT, autoResize);
    }

    public static JTree createTree(Action action) {
        return createSelectableTree(action);
    }

    public static JWindow createWindow() {
        JWindow window = new JWindow();
        return window;
    }

    private static Dimension fieldPreferredHeightSize(Dimension d) {
        d.height = STANDARD_FIELD_HEIGHT;
        return d;
    }

    public static JComponent getCloseButtonPanel(final JFrame frame) {
        JComponent c = new JPanel();
        c.setLayout(new FlowLayout());
        JButton button = createButton(
            new AbstractAction("Close", Icons.getCloseIcon()) {
                public void actionPerformed(ActionEvent event) {
                    ComponentUtilities.closeWindow(frame);
                }
            }
        );
        c.add(button);
        return c;
    }

    private static void initializeAbstractButton(AbstractButton button, Action action) {
        button.addActionListener(action);
        button.setIcon((Icon) action.getValue(Action.SMALL_ICON));
        button.setAlignmentX(0.5f);
        button.setAlignmentY(0.5f);
        button.setText((String) action.getValue(Action.NAME));
        button.setToolTipText((String) action.getValue(Action.SHORT_DESCRIPTION));
        button.setHorizontalTextPosition(AbstractButton.RIGHT);
    }

    public static void removeToolBarButton(FakeToolBar toolBar, JButton button) {
        int nStartComponents = toolBar.getComponentCount();
        toolBar.remove(button);
        int nEndComponents = toolBar.getComponentCount();
        if (nEndComponents != nStartComponents) {
            toolBar.setLayout(new GridLayout(1, nEndComponents));
        }
    }

    private static void setupDragAndDrop(JList list) {

        DragSource.getDefaultDragSource().createDefaultDragGestureRecognizer(list,
                DnDConstants.ACTION_COPY_OR_MOVE, new DefaultListDragSourceListener());
        new DropTarget(list, DnDConstants.ACTION_COPY_OR_MOVE, new ListTarget());
    }

    public static JFrame showInFrame(Component panel, String title) {
        JFrame frame = createFrame();
        frame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
        Container c = frame.getContentPane();
        c.setLayout(new BorderLayout());
        c.add(panel, BorderLayout.CENTER);
        c.add(getCloseButtonPanel(frame), BorderLayout.SOUTH);
        frame.pack();
        frame.setTitle(title);
        ComponentUtilities.center(frame);
        adjustPosition(frame);
        frame.setVisible(true);
        return frame;
    }
}
