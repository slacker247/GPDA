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

import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.util.*;

/**
 * This class is only still here for backward compatibility.  Its inheritance structure
 * is a mess because it adapts the new Widget interface structure to the old
 * single interface structure.
 *
 * @author Ray Fergerson <fergerson@smi.stanford.edu>
 * @deprecated use AbstractSlotWidget or AbstractTabWidget
 */
public abstract class AbstractWidget extends AbstractSlotWidget implements ClsWidget, TabWidget {

    public void initialize() {
    }

    public void layoutLikeCls(edu.stanford.smi.protege.model.Cls cls) {
        Assert.fail("no override");
    }

    public void relayout() {
        Assert.fail("no override");
    }

    public void removeCustomizations() {
        Assert.fail("no override");
    }

    /**
     * setup method comment.
     */
    public void setup(WidgetDescriptor descriptor, Project project) {
        setup(descriptor, false, project, null, null);
    }

    /**
     * setup method comment.
     */
    public void setup(WidgetDescriptor descriptor, boolean isDesignTime, Project project, Cls cls) {
        setup(descriptor, isDesignTime, project, cls, null);
    }
}
