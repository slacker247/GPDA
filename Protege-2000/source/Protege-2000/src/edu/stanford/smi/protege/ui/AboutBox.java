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
import java.io.*;
import javax.swing.*;
import edu.stanford.smi.protege.util.*;
import edu.stanford.smi.protege.resource.*;

/**
 * Panel to display for the "About Box" menu item.
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class AboutBox extends JPanel {

    public AboutBox() {
        String version = Text.getVersion();
        String buildInfo = Text.getBuildInfo();
        Font bigFont = new Font("Dialog", Font.BOLD, 28);
        Font font = new Font("Dialog", Font.PLAIN, 14);
        Font smallFont = new Font("Dialog", Font.PLAIN, 10);

        setLayout(new BorderLayout(0, 20));

        JPanel namePanel = new JPanel(new BorderLayout());
        namePanel.add(createLabel(Text.getProgramName(), bigFont), BorderLayout.EAST);

        JPanel tmPanel = new JPanel(new BorderLayout());
        tmPanel.add(createLabel("TM", font), BorderLayout.NORTH);

        JPanel n1 = new JPanel(new BorderLayout());
        n1.add(namePanel, BorderLayout.CENTER);
        n1.add(tmPanel, BorderLayout.EAST);

        JPanel name = new JPanel(new FlowLayout());
        name.add(n1);

        JPanel center = new JPanel(new BorderLayout());
        center.add(name, BorderLayout.NORTH);
        center.add(createCenteredLabel("Version " + version, new Font("Dialog", Font.PLAIN, 20)), BorderLayout.CENTER);
        center.add(createCenteredLabel(buildInfo, smallFont), BorderLayout.SOUTH);

        JPanel header = new JPanel(new BorderLayout());
        header.add(center, BorderLayout.CENTER);
        header.add(new JLabel(Icons.getNerd32x32Icon()), BorderLayout.WEST);
        add(header, BorderLayout.NORTH);

        JPanel helpPanel = new JPanel(new GridLayout(5, 1));
        helpPanel.add(createLabel("Send questions, bug reports, and suggestions to:", font));
        helpPanel.add(createCenteredText("protege-help@smi.stanford.edu"));
        helpPanel.add(new JLabel(""));
        helpPanel.add(createLabel("For more information see:", font));
        helpPanel.add(createCenteredText("http://protege.stanford.edu"));
        add(helpPanel, BorderLayout.CENTER);

        JPanel legalPanel = new JPanel(new GridLayout(12, 1));
        legalPanel.add(createLabel("Developed by:", font));
        legalPanel.add(createCenteredText("Stanford Medical Informatics"));
        legalPanel.add(createLabel("with support from:", font));
        legalPanel.add(createCenteredText("National Library of Medicine"));
        legalPanel.add(createCenteredText("National Science Foundation"));
        legalPanel.add(createCenteredText("Defense Advanced Research Projects Agency"));
        legalPanel.add(createCenteredText("National Institute of Standards and Technology"));
        legalPanel.add(createLabel("and with additional support from our Affiliates:", font));
        legalPanel.add(createCenteredText("DaimlerChrysler"));
        legalPanel.add(createCenteredText("Fast Track Systems"));
        legalPanel.add(createLabel("", font));
        legalPanel.add(createLabel("Copyright © 1998-2002, Stanford University", smallFont));

        add(legalPanel, BorderLayout.SOUTH);
    }

    private JLabel createCenteredLabel(String text, Font font) {
        JLabel label = createLabel(text, font);
        label.setHorizontalAlignment(JLabel.CENTER);
        return label;
    }

    private JComponent createCenteredText(String text) {
        JTextField field = new JTextField(text);
        field.setEnabled(false);
        field.setBackground(getBackground());
        field.setDisabledTextColor(getForeground());
        field.setBorder(null);
        field.setHorizontalAlignment(JTextField.CENTER);
        return field;
    }

    private JLabel createLabel(String text, Font font) {
        JLabel label = new JLabel(text);
        label.setFont(font);
        return label;
    }
}
