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
import javax.swing.*;
import edu.stanford.smi.protege.*;
import edu.stanford.smi.protege.action.*;
import edu.stanford.smi.protege.resource.*;
import edu.stanford.smi.protege.util.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class ProjectToolBar extends JToolBar {

    public ProjectToolBar() {
        setFloatable(false);

        addButton(new CreateProject());
        addButton(new OpenProject());
        addButton(new SaveProject());
        addSeparator();
        /*
         * addButton(new DefaultAction("Cut", Icons.getCutIcon()));
         * addButton(new DefaultAction("Copy", Icons.getCopyIcon()));
         * addButton(new DefaultAction("Paste", Icons.getPasteIcon()));
         * addButton(new DefaultAction("Delete", Icons.getDeleteIcon()));
         * addSeparator();
         */
        /*
        addButton(new UndoAction());
        addButton(new RedoAction());
        addSeparator();
        */
        /*
         * addButton(new DefaultAction("Find", Icons.getFindIcon()));
         * addButton(new DefaultAction("Find Previous", Icons.getFindPreviousIcon()));
         * addButton(new DefaultAction("Find Next", Icons.getFindNextIcon()));
         * addSeparator();
         */
        addButton(new CascadeWindows());
        addButton(new CloseAllWindows());
        // addSeparator();
        // addButton(new PrintDebugMessages(manager));
    }

    public void addButton(Action action) {
        ComponentFactory.addToolBarButton(this, action);
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
