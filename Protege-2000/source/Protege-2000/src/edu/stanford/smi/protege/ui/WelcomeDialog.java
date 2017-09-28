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
import java.awt.*;
import java.awt.event.*;
import java.util.ArrayList;
import java.util.HashMap;
// javax
import javax.swing.*;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.border.TitledBorder;
// protege
import edu.stanford.smi.protege.model.KnowledgeBaseFactory;
import edu.stanford.smi.protege.resource.Icons;
import edu.stanford.smi.protege.resource.Text;
import edu.stanford.smi.protege.util.ApplicationProperties;
import edu.stanford.smi.protege.util.ComponentFactory;
import edu.stanford.smi.protege.util.SystemUtilities;
import edu.stanford.smi.protege.util.WaitCursor;

/**
 * Startup dialog that's displayed when a user starts Protege-2000 (without
 * double clicking on a project file).  The dialog is displayed on top of the
 * main window and gives the user the option to create a new project, open
 * a recently used project, or launch one of several help topics.
 *
 * @author Jennifer Vendetti
 */
 public class WelcomeDialog extends JDialog {

    // Top level panel.
    JPanel panel = new JPanel(new BorderLayout(10, 0));

    // New Project panel and sub-panels.
    JPanel newPanel = new JPanel(new BorderLayout());
    JPanel ptpHolder = new JPanel(new FlowLayout());
    JPanel projectTypePanel;
    JPanel newButtonPanel = new JPanel(new FlowLayout());

    // Open Project panel and sub-panels.
    JPanel openPanel = new JPanel(new BorderLayout());
    JPanel mruPanel = new JPanel(new BorderLayout());
    JPanel openButtons = new JPanel(new FlowLayout());
    JPanel openButtonPanel = new JPanel(new GridLayout(1, 2));

    // Help Resources panel and sub-panels.
    JPanel helpPanel = new JPanel(new BorderLayout());
    JPanel hspHolder = new JPanel(new FlowLayout());
    JPanel helpSubPanel = new JPanel(new GridLayout(4, 1, 0, 2));
    JPanel iconPanel = new JPanel(new GridLayout(1, 1));

    TitledBorder titledBorder;
    TitledBorder titledBorder1;
    TitledBorder titledBorder2;

    JButton newButton = new JButton("New");
    JButton openOtherButton = new JButton("Open Other...");
    JButton openButton = new JButton("Open");
    JButton topicsButton = new JButton("All Topics");
    JButton tutorialButton = new JButton("Getting Started");
    JButton usersGuideButton = new JButton("User\'s Guide");
    JButton faqButton = new JButton("FAQ");

    ButtonGroup group = new ButtonGroup();
    ProjectList mruList;
    JScrollPane mruScrollPane;
    JLabel iconLabel = new JLabel();
    JRadioButton[] rbArray;
    java.util.List factoryList;
    java.util.List projectList = new ArrayList(ApplicationProperties.getMRUProjectList());

    // Extended the JList class in order to have tool tips for each
    // individual list item.
    private class ProjectList extends JList {
        DefaultListModel model = new DefaultListModel();

        public ProjectList(DefaultListModel model) {
            this.model = model;
            this.setModel(model);
            ToolTipManager.sharedInstance().registerComponent(this);
        }

        public String getToolTipText(MouseEvent event) {
            String key = null, toolTip = null;
            int index = locationToIndex(event.getPoint());
            if (index >= 0) {
                toolTip = (String) projectList.get(index);
            }
            return toolTip;
        }
    }

    public WelcomeDialog() {
        this(null, "", false);
    }

    public WelcomeDialog(java.awt.Frame frame, String title, boolean modal) {
        super(frame, title, modal);
        try {
            this.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
            jbInit();
            pack();
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }

    void jbInit() throws Exception {
        titledBorder = new TitledBorder(BorderFactory.createEtchedBorder(), "New Project");
        titledBorder1 = new TitledBorder(BorderFactory.createEtchedBorder(), "Open Recent Project");
        titledBorder2 = new TitledBorder(BorderFactory.createEtchedBorder(), "Help");
        ptpHolder.setBorder(titledBorder);
        mruPanel.setBorder(titledBorder1);
        hspHolder.setBorder(titledBorder2);

        // Initialize JLabel that holds the Protege-2000 icon.
        iconLabel.setIcon(Icons.getNerd32x32Icon());
        iconLabel.setHorizontalAlignment(SwingConstants.CENTER);
        iconLabel.setVerticalAlignment(SwingConstants.CENTER);
        iconLabel.setHorizontalTextPosition(SwingConstants.CENTER);
        iconLabel.setVerticalTextPosition(SwingConstants.BOTTOM);
        iconLabel.setFont(new java.awt.Font("Dialog", Font.BOLD, 12));
        iconLabel.setIconTextGap(5);
        iconLabel.setText(Text.getProgramName());

        /* Build New Project panel ********************************************/
        ptpHolder.add(getProjectTypePanel());
        newPanel.add(ptpHolder, BorderLayout.CENTER);

        newButtonPanel.add(newButton);
        newButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent ae) {
                newButton_actionPerformed(ae);
            }
        });
        newPanel.add(newButtonPanel, BorderLayout.SOUTH);

        /* Build Open Project panel *******************************************/
        initList();
        mruScrollPane = new JScrollPane(mruList);
        mruPanel.add(mruScrollPane, BorderLayout.CENTER);

        openPanel.add(mruPanel, BorderLayout.CENTER);

        openButton.setEnabled(false);
        openButtonPanel.add(openButton);
        openButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent ae) {
                openButton_actionPerformed(ae);
            }
        });

        openButtonPanel.add(openOtherButton);
        openOtherButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent ae) {
                openOtherButton_actionPerformed(ae);
            }
        });

        openButtons.add(openButtonPanel);
        openPanel.add(openButtons, BorderLayout.SOUTH);

        /* Build Help Resources panel *****************************************/
        helpSubPanel.add(tutorialButton);
        tutorialButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent ae) {
                tutorialButton_actionPerformed(ae);
            }
        });

        helpSubPanel.add(faqButton);
        faqButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent ae) {
                faqButton_actionPerformed(ae);
            }
        });

        helpSubPanel.add(usersGuideButton);
        usersGuideButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent ae) {
                usersGuideButton_actionPerformed(ae);
            }
        });

        helpSubPanel.add(topicsButton);
        topicsButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent ae) {
                topicsButton_actionPerformed(ae);
            }
        });

        hspHolder.add(helpSubPanel);
        helpPanel.add(hspHolder, BorderLayout.CENTER);

        iconPanel.add(iconLabel);
        helpPanel.add(iconPanel, BorderLayout.SOUTH);

        /* Build main dialog **************************************************/
        panel.add(newPanel, BorderLayout.WEST);
        panel.add(openPanel, BorderLayout.CENTER);
        panel.add(helpPanel, BorderLayout.EAST);
        this.getContentPane().add(panel);
    }

    private JPanel getProjectTypePanel() {
        // Build the top section of the "New Project" panel:

        // Get a collection of the available project types.
        // a.k.a. back-ends
        // a.k.a. knowledge-base factories
        factoryList = new ArrayList(SystemUtilities.getAvailableFactories());

        projectTypePanel = new JPanel(new GridLayout(factoryList.size(), 1, 0, 2));

        // Create a radio button for each new project type and assign all the
        // buttons to a radio group.
        group = new ButtonGroup();
        rbArray = new JRadioButton[factoryList.size()];
        for (int i = 0; i < factoryList.size(); i++) {
            KnowledgeBaseFactory factory = (KnowledgeBaseFactory) factoryList.get(i);
            JRadioButton rb = new JRadioButton(factory.getDescription());
            rbArray[i] = rb;
            group.add(rb);
            projectTypePanel.add(rb);
            //System.out.println(rbArray[i].getText());
        }
        rbArray[0].setSelected(true);
        return projectTypePanel;
    }

    private void initList() {
        DefaultListModel model = new DefaultListModel();

        // Populate the list's model with data.
        String projectName = null, projectPath = null;
        for (int i = 0; i < projectList.size(); i++) {
            projectPath = (String) projectList.get(i);
            int index = projectPath.lastIndexOf(java.io.File.separatorChar);
            projectName = projectPath.substring(index + 1);
            projectName = projectName.substring(0, projectName.indexOf(".pprj"));
            model.addElement(projectName);
        }

        mruList = new ProjectList(model);
        mruList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);

        mruList.addListSelectionListener(new ListSelectionListener() {
            public void valueChanged(ListSelectionEvent e) {
                if (e.getValueIsAdjusting()) { return; }
                if (!mruList.isSelectionEmpty()) {
                    openButton.setEnabled(true);
                }
            }
        });

        mruList.addMouseListener(new MouseAdapter() {
            public void mouseClicked(MouseEvent e) {
                if (e.getClickCount() == 2) {
                    int index = mruList.locationToIndex(e.getPoint());
                    doOpenProject(index);
                }
            }
        });
    }

    private void doOpenProject(int index) {
        String filePath = (String) projectList.get(index);
        if ((filePath != null) && (!filePath.equals(""))) {
            WaitCursor cursor = new WaitCursor(this.getRootPane());
            cursor.show();
            this.setVisible(false);
            ProjectManager.getProjectManager().loadProject(filePath);
            ApplicationProperties.addProjectToMRUList(filePath);
            cursor.hide();
        }
    }

    public void newButton_actionPerformed(ActionEvent ae) {
        WaitCursor cursor = new WaitCursor(this.getRootPane());
        cursor.show();
        // Find out which radio button is selected in the "New Project"
        // panel and open the appropriate project type.
        for (int i = 0; i < rbArray.length; i++) {
            if (rbArray[i].isSelected()) {
                KnowledgeBaseFactory factory = (KnowledgeBaseFactory) factoryList.get(i);
                ProjectManager.getProjectManager().loadProject(null, factory);
            }
        }
        cursor.hide();
        this.setVisible(false);
    }

    public void openButton_actionPerformed(ActionEvent ae) {
        int index = mruList.getSelectedIndex();
        doOpenProject(index);
    }

    public void openOtherButton_actionPerformed(ActionEvent ae) {
        // Don't want to use this line of code because it opens
        // the JFileChooser dialog with the application's main frame as the
        // parent.  If the main frame is the parent and the user clicks the
        // Cancel button, focus goes to the main frame instead of back to
        // the welcome dialog.
        //ProjectManager.getProjectManager().openProjectRequest();

        String filePath;
        JFileChooser chooser = ComponentFactory.createFileChooser("Project", ".pprj");
        int rval = chooser.showOpenDialog(this);
        if (rval == JFileChooser.APPROVE_OPTION) {
            filePath = chooser.getSelectedFile().getPath();
            WaitCursor cursor = new WaitCursor(this.getRootPane());
            cursor.show();
            this.setVisible(false);
            ProjectManager.getProjectManager().loadProject(filePath);
            ApplicationProperties.addProjectToMRUList(filePath);
            cursor.hide();
        }

    }

    public void faqButton_actionPerformed(ActionEvent ae) {
        SystemUtilities.showHTML("http://protege.stanford.edu/faq.shtml");
    }

    public void topicsButton_actionPerformed(ActionEvent ae) {
        SystemUtilities.showHTML("http://protege.stanford.edu/useit.shtml");
    }

    public void tutorialButton_actionPerformed(ActionEvent ae) {
        char slash = java.io.File.separatorChar;
        SystemUtilities.showHTML("doc" + slash + "tutorial" + slash + "getting_started" + slash + "index.html");
    }

    public void usersGuideButton_actionPerformed(ActionEvent ae) {
        char slash = java.io.File.separatorChar;
        SystemUtilities.showHTML("doc" + slash + "users_guide" + slash + "index.html");
    }
}
