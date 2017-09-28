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
import javax.swing.*;
import edu.stanford.smi.protege.resource.Icons;

/**
 * Dialog that displays a list of Protege-2000 icons with descriptions.
 *
 * @author    Jennifer L. Vendetti
 */

public class IconDialog extends JDialog {
    // Initialize array of Protege-2000 icons to display in this dialog.
    Icon[] icons = { null, Icons.getAddIcon(), Icons.getRemoveIcon(),
                     Icons.getCreateIcon(), Icons.getDeleteIcon(),
                     Icons.getViewIcon(), Icons.getUpIcon(), Icons.getUpIcon(),
                     Icons.getDownIcon(), Icons.getCreateYellowStickyIcon(),
                     Icons.getDeleteYellowStickyIcon(),
                     null, Icons.getClsIcon(), Icons.getClsMetaClsIcon(),
                     Icons.getDefaultIcon(),
                     Icons.getHiddenClsIcon(), Icons.getAbstractClsIcon(),
                     Icons.getMultipleParentsIcon(), Icons.getFacetIcon(),
                     Icons.getInstanceIcon(), Icons.getSlotIcon(),
                     Icons.getInheritedSlotIcon(),
                     Icons.getViewTopLevelSlotIcon(),
                     Icons.getViewSlotAtClassIcon(), Icons.getInverseSlotIcon(),
                     Icons.getDirectOverriddenSlotIcon(),
                     Icons.getOverriddenSlotIcon(),
                     Icons.getRemoveSlotOverrideIcon(), null,
                     Icons.getFormIcon(), Icons.getRelayoutIcon(),
                     Icons.getLayoutLikeOtherFormIcon(),
                     Icons.getCustomizedFormIcon(),
                     Icons.getRemoveCustomizationsIcon() };

    // Initialize array of textual descriptions for above icons.
    String[] iconText = { "General Icons", "Add, attach, or select",
                          "Remove selected object from view but not from the knowledge base",
                          "Create a class, slot, or instance",
                          "Delete selected class, slot, or instance from the knowledge base",
                          "View selected class, slot, or instance",
                          "Display references to selected class, slot, or instance",
                          "Move selected item up in list",
                          "Move selected item down in list",
                          "Create note", "Delete note", "Frame Icons", "Class",
                          "Metaclass", "Default slot metaclass or default facet metaclass", "Hidden class",
                          "Abstract class (a class with no instances)",
                          "Class with multiple parents (multiple inheritance)",
                          "Facet", "Instance", "Slot", "Inherited slot",
                          "View selected slots", "View selected slots at class",
                          "Slot with an inverse slot", "Overriden direct slot",
                          "Overriden inherited slot", "Remove slot override",
                          "Form Icons", "Form",
                          "Default layout with current widgets",
                          "Layout like form...", "Customized Form",
                          "Remove all customizations" };

    public IconDialog() {
        this(null, "", false);
    }

    public IconDialog(Frame frame, String title, boolean modal) {
        super(frame, title, modal);
        try {
            init();
            pack();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void closeButton_actionPerformed(ActionEvent ae) {
        this.setVisible(false);
    }

    private int getMaxHeight() {
        int maxHeight = 0;
        for (int i = 0; i < icons.length; i++) {
            if (icons[i] != null) {
                int height = icons[i].getIconHeight();
                if (height > maxHeight) {
                    maxHeight = height;
                }
            }
        }
        return maxHeight;
    }

    private int getMaxWidth() {
        int maxWidth = 0;
        for (int i = 0; i < icons.length; i++) {
            if (icons[i] != null) {
                int width = icons[i].getIconWidth();
                if (width > maxWidth) {
                    maxWidth = width;
                }
            }
        }
        return maxWidth;
    }

    private JTextField getTextHeader(String text) {
        JTextField textField = new JTextField(text);
        textField.setEnabled(false);
        textField.setBackground(getBackground());
        textField.setDisabledTextColor(getForeground());
        textField.setBorder(null);
        textField.setFont(new Font("Dialog", Font.BOLD, 14));
        return textField;
    }

    private void init() throws Exception {
        JPanel iconPanel = new JPanel(new GridLayout(icons.length, 1));

        // Get the max width and height of the icons to display.
        int maxWidth = getMaxWidth();
        int maxHeight = getMaxHeight();
        Dimension d = new Dimension(maxWidth, maxHeight);

        // Populate grid layout with icons and their descriptions.
        for (int i = 0; i < icons.length; i++) {
            JPanel panel = new JPanel(new BorderLayout(15, 0));

            if (icons[i] == null) {
                // This will be a section header.
                JTextField header = getTextHeader(iconText[i]);
                panel.add(header, BorderLayout.CENTER);
            } else {
                // Create a label for the icon.
                JLabel icon = new JLabel(icons[i], JLabel.CENTER);
                icon.setPreferredSize(d);
                panel.add(icon, BorderLayout.WEST);

                // Create a label for the icon description.
                JLabel iconDescription = new JLabel(iconText[i], JLabel.LEFT);
                panel.add(iconDescription, BorderLayout.CENTER);
            }
            iconPanel.add(panel);
        }

        // Put icon grid in a scroll pane.
        JScrollPane scrollPane = new JScrollPane(iconPanel);
        scrollPane.setViewportBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

        // Build button panel for the "Close" button.
        JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.CENTER));
        JButton closeButton = new JButton("Close");
        closeButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent ae) {
                closeButton_actionPerformed(ae);
            }
        });
        buttonPanel.add(closeButton);

        this.getContentPane().setLayout(new BorderLayout());
        this.getContentPane().add(scrollPane, BorderLayout.CENTER);
        this.getContentPane().add(buttonPanel, BorderLayout.SOUTH);
    }
}
