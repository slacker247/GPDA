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


import javax.swing.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.resource.*;
import edu.stanford.smi.protege.util.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class FormRenderer extends DefaultRenderer {
    private static Icon _icon = Icons.getFormIcon();
    private static Icon _customizedFormIcon = Icons.getCustomizedFormIcon();
    private Project _project;

    public FormRenderer(Project project) {
        _project = project;
    }

    public void load(Object o) {
        setMainIcon(_icon);
        if (o instanceof Cls) {
            Cls cls = (Cls) o;
            setMainText(cls.getBrowserText());
            if (_project.hasCustomizedDescriptor(cls)) {
                appendIcon(_customizedFormIcon);
            }
        }
    }
}
