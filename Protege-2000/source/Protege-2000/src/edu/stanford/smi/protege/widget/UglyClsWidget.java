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
import java.util.*;
import javax.swing.*;
import edu.stanford.smi.protege.model.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class UglyClsWidget extends AbstractClsWidget {

    public String getLabel() {
        return "Ugly Cls Widget (tm)";
    }

    public void initialize() {
        JComponent c = new JLabel("The Ugly Cls Widget (tm)", JLabel.CENTER);
        c.setOpaque(true);
        c.setBackground(Color.red);
        c.setForeground(Color.black);
        c.setBorder(BorderFactory.createMatteBorder(5, 5, 5, 5, Color.green));
        add(c);
        setPreferredColumns(2);
        setPreferredRows(2);
    }

    /**
     * Insert the method's description here.
     * Creation date: (8/17/2000 4:12:16 PM)
     * @param cls edu.stanford.smi.protege.model.Cls
     */
    public void layoutLikeCls(Cls cls) {}

    /**
     * Insert the method's description here.
     * Creation date: (8/17/2000 4:12:31 PM)
     */
    public void relayout() {}

    /**
     * Insert the method's description here.
     * Creation date: (8/17/2000 4:12:00 PM)
     */
    public void removeCustomizations() {}
}
