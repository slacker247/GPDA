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

package edu.stanford.smi.protege.storage.clips;

import java.awt.*;
import java.io.*;
import java.util.*;
import javax.swing.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class ParseErrorPanel extends JComponent {

    public ParseErrorPanel(Collection errors) {
        setLayout(new BorderLayout());
        JTextArea area = new JTextArea();
        add(new JScrollPane(area));
        area.setText(getText(errors));
        setPreferredSize(new Dimension(700, 400));
    }

    private String getText(Collection errors) {
        StringBuffer buffer = new StringBuffer();
        Iterator i = errors.iterator();
        while (i.hasNext()) {
            String text;
            Object o = i.next();
            if (o instanceof Exception) {
                Exception e = (Exception) o;
                StringWriter s = new StringWriter();
                e.printStackTrace(new PrintWriter(s));
                text = s.toString();
            } else {
                text = o.toString();
            }
            buffer.append(text);
            buffer.append("\n");
        }
        return buffer.toString();
    }
}
