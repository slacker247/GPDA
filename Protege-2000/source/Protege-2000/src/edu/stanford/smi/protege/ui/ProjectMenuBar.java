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

// java
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import java.util.*;

// protege
import edu.stanford.smi.protege.*;
import edu.stanford.smi.protege.action.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.resource.*;
import edu.stanford.smi.protege.util.*;

/**
 *
 * @author Ray Fergerson
 * @author Jennifer Vendetti
 */
public class ProjectMenuBar extends JMenuBar {

    private ButtonGroup _group = new ButtonGroup();
    private Map fullPaths = new HashMap();
    private int MAX_LENGTH = 50;
    private char sep = java.io.File.separatorChar;

    public ProjectMenuBar() {
        createProjectMenu();
        createEditMenu();
        createWindowMenu();
        createHelpMenu();
    }

    private void createEditMenu() {
        JMenu menu = ComponentFactory.createMenu();
        menu.setText("Edit");
        // createItem(menu, new UndoAction());
        // createItem(menu, new RedoAction());
        // createItem(menu, new ShowCommandHistoryAction());
        add(menu);
    }

    private void createHelpMenu() {
        JMenu menu = ComponentFactory.createMenu();
        menu.setText("Help");
        add(menu);

        // getting started
        ComponentFactory.addMenuItemNoIcon(menu,
            new DisplayHtml("Getting Started",
            "doc" + sep + "tutorial" + sep + "getting_started" + sep + "index.html"));
        // frequently asked questions
        ComponentFactory.addMenuItemNoIcon(menu,
            new DisplayHtml("Frequently Asked Questions",
            "http://protege.stanford.edu/faq.shtml"));
        // user's guide
        ComponentFactory.addMenuItemNoIcon(menu,
            new DisplayHtml("User\'s Guide",
            "doc" + sep + "users_guide" + sep + "index.html"));
        // ontology development 101
        ComponentFactory.addMenuItemNoIcon(menu,
            new DisplayHtml("Ontology Development 101",
            "http://protege.stanford.edu/publications/ontology_development/ontology101.shtml"));
        // advanced widgets submenu
        JMenu subMenu = new JMenu("Advanced Widgets");
        // diagram widget
        ComponentFactory.addMenuItemNoIcon(subMenu,
            new DisplayHtml("Diagram Widget",
            "doc" + sep + "tutorial" + sep + "diagrams" + sep + "index.html"));
        // table widget
        ComponentFactory.addMenuItemNoIcon(subMenu,
            new DisplayHtml("Table Widget",
            "doc" + sep + "tutorial" + sep + "tables" + sep + "index.html"));
        menu.add(subMenu);
        menu.addSeparator();

        ComponentFactory.addMenuItemNoIcon(menu, new ShowIconDialog());
        menu.addSeparator();

        ComponentFactory.addMenuItemNoIcon(menu, new ShowAboutBox());
    }

    private void createItem(JMenu menu, Action action) {
        ComponentFactory.addMenuItem(menu, action);
    }

    private JMenuItem createItemX(UIManager.LookAndFeelInfo lf) {
        Action action = new LookAndFeelAction(lf);
        JRadioButtonMenuItem item = ComponentFactory.createRadioButtonMenuItem(action);
        _group.add(item);
        if (lf.getClassName().equals(UIManager.getLookAndFeel().getClass().getName())) {
            _group.setSelected(item.getModel(), true);
        }
        return item;
    }

    private JMenu createLAFMenu() {
        JMenu menu = ComponentFactory.createMenu();
        menu.setText("Look And Feel");

        UIManager.LookAndFeelInfo[] lookAndFeels = UIManager.getInstalledLookAndFeels();
        for (int i = 0; i < lookAndFeels.length; ++i) {
            menu.add(createItemX(lookAndFeels[i]));
        }
        return menu;
    }

    private void createProjectMenu() {
        final JMenu menu = ComponentFactory.createMenu();
        menu.setText("Project");
        menu.addMenuListener(new MenuListener () {
            public void menuCanceled(MenuEvent e) {
            }
            public void menuSelected(MenuEvent e) {
                loadProjectMenu(menu);
            }
            public void menuDeselected(MenuEvent e) {
            }
        });
        add(menu);
    }

    private void loadProjectMenu(JMenu menu) {
        menu.removeAll();
        createItem(menu, new CreateProject());
        createItem(menu, new OpenProject());
        loadOpenRecent(menu);
        createItem(menu, new SaveProject());
        createItem(menu, new SaveAsProject());
        createItem(menu, new CloseProject());
        menu.addSeparator();
        createItem(menu, new BuildProject());
        createItem(menu, new ChangeProjectStorageFormat());
        menu.addSeparator();
        createItem(menu, new IncludeProject());
        createItem(menu, new ShowIncludedProjects());
        createItem(menu, new MergeIncludedProjects());
        menu.addSeparator();
        createItem(menu, new ConfigureProject());
        createItem(menu, new ShowMetrics());
        createItem(menu, new GenerateHtml());
        menu.addSeparator();
        createItem(menu, new ExitApplication());
    }

    private void loadOpenRecent(JMenu menu) {
        JMenu subMenu = new JMenu("Open Recent");
        subMenu.setIcon(Icons.getBlankIcon());
        fullPaths.clear();

        java.util.List projectList = ApplicationProperties.getMRUProjectList();
        for (int i=0; i<projectList.size(); i++) {
            String fullPath = (String) projectList.get(i);
            String text = fullPath;

            // If the file path is too long to display, reduce it.
            if (text.length() > MAX_LENGTH) {
                ArrayList list = buildList(text);
                list = reduce(list);
                text = rebuildPath(list);
            }

            // Map display text to full path.
            fullPaths.put(text, fullPath);

            JMenuItem menuItem = new JMenuItem(text);
            menuItem.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    JMenuItem source = (JMenuItem)(e.getSource());
                    String fileName = source.getText();
                    boolean b = ProjectManager.getProjectManager().closeProjectRequest();
                    if (b == true) {
                        String path = (String) fullPaths.get(fileName);
                        ProjectManager.getProjectManager().loadProject(path);
                        ApplicationProperties.addProjectToMRUList(path);
                    }
                }
            });
            subMenu.add(menuItem);
        }
        menu.add(subMenu);
    }

    private ArrayList buildList(String path) {
        ArrayList list = new ArrayList();
        StringTokenizer st = new StringTokenizer(path, java.io.File.separator);
        for (int i=0; st.hasMoreElements(); i++) {
          list.add(i, st.nextElement());
        }
        return list;
    }

    private ArrayList reduce(ArrayList list) {
        // sanity checks
        if (list == null) { return null; }
        if (list.size() == 0) {return list; }
        if (getLength(list) <= MAX_LENGTH) { return list; }

        // size is 1
        if (list.size() == 1) {
            String fname = (String) list.get(0);
            fname = fname.substring(0, (MAX_LENGTH - 3)) + "...";
            list.set(0, fname);
        }
        // size is 2
        else if (list.size() == 2) {
            if (list.get(0) == "...") {
                list.remove(0);
            }
            else {
                list.set(0, "...");
            }
        }
        // size is 3 or more
        else {
            if (!list.contains("...")) {
                int mid = list.size() / 2;
                list.set(mid, "...");
            } else {
                int idxDot = list.indexOf("...");
                int before = idxDot;
                int after = (list.size() - 2) - idxDot;

                if (before > after) {
                    list.remove(idxDot - 1);
                } else {
                    // since we always remove "after" when before == after, we are
                    // sure that before is always equal to or one more than before.
                    list.remove(idxDot + 1);
                }
            }
        }

        if (getLength(list) <= MAX_LENGTH) {
            // list is short enough
            return list;
        } else {
            // list is too long - recurse
            return reduce(list);
        }
    }

    private int getLength(ArrayList list) {
        String path = "";
        String sep = java.io.File.separator;
        for (int i=0; i<list.size(); i++) {
            path = path + list.get(i) + sep;
        }
        return path.length() - 1;
    }

    private String rebuildPath(ArrayList list) {
        String path = "";
        String sep = java.io.File.separator;
        for (int i=0; i<list.size(); i++) {
            path = path + list.get(i) + sep;
        }
        path = path.substring(0, (path.length() - 1));
        return path;
    }

    private void createWindowMenu() {
        final JMenu menu = ComponentFactory.createMenu();
        menu.setText("Window");
        menu.addMenuListener(
            new MenuListener() {
                public void menuSelected(MenuEvent event) {
                    loadWindowMenu(menu);
                }

                public void menuCanceled(MenuEvent event) {
                }

                public void menuDeselected(MenuEvent event) {
                }
            }
        );
        add(menu);
    }

    private void loadWindowMenu(JMenu windowMenu) {
        windowMenu.removeAll();
        createItem(windowMenu, new IncreaseFontSize());
        createItem(windowMenu, new DecreaseFontSize());
        createItem(windowMenu, new CascadeWindows());
        createItem(windowMenu, new CloseAllWindows());
        ComponentFactory.addSubmenu(windowMenu, createLAFMenu());
        windowMenu.addSeparator();
        Project project = ProjectManager.getProjectManager().getCurrentProject();
        if (project != null) {
            Iterator i = project.getOpenWindows().iterator();
            while (i.hasNext()) {
                JFrame jframe = (JFrame) i.next();
                createItem(windowMenu, new JFrameToFront(jframe));
            }
        }
    }
}