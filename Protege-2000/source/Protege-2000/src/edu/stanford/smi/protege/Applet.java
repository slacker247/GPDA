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

package edu.stanford.smi.protege;

import java.awt.*;
import java.awt.event.*;
import java.net.*;
import javax.swing.*;
import edu.stanford.smi.protege.resource.*;
import edu.stanford.smi.protege.util.*;

/**
 * Class to allow Protege to run as an applet.  This is used for demos.
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class Applet extends JApplet {
    static {
        SystemUtilities.init();
    }

    private String getProjectName() {
        return getParameter("project_name");
    }

    public void init() {
        try {
            String projectName = getProjectName();
            if (projectName != null) {
                URL baseURL = new URL(getCodeBase(), projectName);
                FileUtilities.setBaseURL(baseURL);
            }
            setup(projectName);
        } catch (Exception e) {
            Log.exception(e, this, "init");
            SystemUtilities.pause();
        }
    }

    public static void main(String[] args) {
        JFrame frame = new JFrame();
        frame.getContentPane().setLayout(new BorderLayout());
        Applet applet = new Applet();
        frame.getContentPane().add(applet);
        applet.setup(args[0]);
        frame.pack();
        frame.show();
    }

    private void setup(final String projectName) {
        JButton button = new JButton(Icons.getNerd32x32Icon());
        button.setFocusPainted(false);
        button.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent event) {
                String[] args;
                if (projectName == null) {
                    args = new String[] {
                    };
                } else {
                    args = new String[] { projectName };
                }
                Application.main(args);
            }
        });
        getContentPane().add(button);
    }
}
