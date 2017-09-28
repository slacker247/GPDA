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

package edu.stanford.smi.protegex.queries_tab;

import java.awt.*;
import javax.swing.*;
import javax.swing.tree.*;
import edu.stanford.smi.protege.*;
import edu.stanford.smi.protege.resource.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.model.Frame;
import edu.stanford.smi.protege.widget.*;
import edu.stanford.smi.protege.util.*;
import edu.stanford.smi.protegex.queries_tab.toolbox.*;
import edu.stanford.smi.protege.ui.*;

public class QueriesTabRenderer extends DefaultRenderer {
      private static final int NONE = 0;
      private static final int DIRECT = 1;
      private static final int ALL = 2;

    private static Icon theClsIcon = Icons.getClsIcon();
    private static Icon theMetaClsIcon = Icons.getClsMetaClsIcon();
    private static Icon theReadonlyClsIcon = Icons.getReadonlyClsIcon();
    private static Icon theReadonlyMetaClsIcon = Icons.getReadonlyMetaClsIcon();
    private static Icon theAbstractClsIcon = Icons.getAbstractClsIcon();
    private static Icon theReadonlyAbstractClsIcon = Icons.getReadonlyAbstractClsIcon();
    private static Icon theMultipleParentsIcon = Icons.getMultipleParentsIcon();
    private static Icon theReadonlyMultipleParentsIcon = Icons.getReadonlyMultipleParentsIcon();
    private static Icon theHiddenClsIcon = Icons.getHiddenClsIcon();
    private static Icon theReadonlyHiddenClsIcon = Icons.getReadonlyHiddenClsIcon();
    private static Icon theInstanceIcon = Icons.getInstanceIcon();
    private static Icon theReadonlyInstanceIcon = Icons.getReadonlyInstanceIcon();
    private static Icon theSlotIcon = Icons.getSlotIcon();
    private static Icon theReadonlySlotIcon = Icons.getReadonlyDirectSlotIcon();
    private static Icon theFacetIcon = Icons.getFacetIcon();
    private static Icon theReadonlyFacetIcon = Icons.getReadonlyFacetIcon();
    private static Icon theDefaultIcon = Icons.getDefaultIcon();
    private static Icon theReadonlyDefaultIcon = Icons.getReadonlyDefaultIcon();

    private static Icon theQueryIcon = Helper.getIcon("Query");
    private static Icon theReadonlyQueryIcon = Icons.getQueryIcon();

    private boolean hasLoadedIconFlags = false;
      private int itsInstanceCountType = NONE;
    private int itsStringType = NONE;
      private boolean displayFrameTypeIcon = true;
      private boolean displayAbstractIcon = true;
      private boolean displayMultipleParentsIcon = true;
      private boolean displayDefaultMetaclassIcon = true;

    private void ensureIconFlagsLoaded() {
        if (!hasLoadedIconFlags) {
            //Project p = Application.getCurrentProject();
            Project p = ProjectManager.getProjectManager().getCurrentProject();
            if (p != null) {
                displayAbstractIcon = p.getDisplayAbstractClassIcon();
                displayMultipleParentsIcon = p.getDisplayMultiParentClassIcon();
                hasLoadedIconFlags = true;
            }
        }
    }

    private String getInstanceCountString(Cls cls) {
        int count;
        switch (itsInstanceCountType) {
            case NONE :
                count = 0;
                break;
            case DIRECT :
                count = cls.getDirectInstanceCount();
                break;
            case ALL :
                count = cls.getInstanceCount();
                break;
            default :
                //Assert.unreachable(itsInstanceCountType);
                count = 0;
        }
        String s;
        if (count > 0) {
            s = "  (" + count + ")";
        } else {
            s = "";
        }
        return s;
    }

    // This is the one added by Qi
    private String getOtherString(Cls cls) {
        String str;
        switch (itsStringType) {
            case NONE :
                str = "";
                break;
            case DIRECT :
                str = cls.getBrowserText();
                break;
            default :
                str = "";
        }
        String s;
        if (str.length() > 1) {
            s = "  (" + str + ")";
        } else {
            s = "";
        }
        return s;
    }

    // This is the one added by Qi
    private String getOtherString(Instance instance) {
        String str;
        switch (itsStringType) {
            case NONE :
                str = "";
                break;
            case DIRECT :
                str = instance.getDirectType().getName();
                break;
            default :
                str = "";
        }
        String s;
        if (str.length() > 1) {
            s = "  (" + str + ")";
        } else {
            s = "";
        }
        return s;
    }

    public void load(Object value) {
        ensureIconFlagsLoaded();

        if (value instanceof Frame) {
            if (!((Frame) value).isEditable()) {
                setGrayedText(true);
            }
        }

        if (value instanceof Cls) {
            loadCls((Cls) value);
        } else if (value instanceof Slot) {
            loadSlot((Slot) value);
        } else if (value instanceof Facet) {
            loadFacet((Facet) value);
        } else if (value instanceof Instance) {
            loadInstance((Instance) value);
        } else if (value instanceof InstancesQuery) {
            loadQuery((InstancesQuery) value);
        } else {
            setMainIcon(null);
            setMainText(value.toString());
        }
    }

    private void loadCls(Cls cls) {
        boolean editable = cls.isEditable();
        if (cls.isClsMetaCls()) {
            setMainIcon(editable ? theMetaClsIcon : theReadonlyMetaClsIcon);
        } else {
            setMainIcon(editable ? theClsIcon : theReadonlyClsIcon);
        }
        setMainText(cls.getBrowserText());
        if (displayAbstractIcon && cls.isAbstract()) {
            appendIcon(editable ? theAbstractClsIcon : theReadonlyAbstractClsIcon);
        }
        if (displayMultipleParentsIcon && cls.getDirectSuperclasses().size() > 1) {
            appendIcon(editable ? theMultipleParentsIcon : theReadonlyMultipleParentsIcon);
        }
        if (displayDefaultMetaclassIcon && (cls.isDefaultSlotMetaCls() || cls.isDefaultFacetMetaCls())) {
            appendIcon(editable ? theDefaultIcon : theReadonlyDefaultIcon);
        }
        if (!cls.isVisible()) {
            appendIcon(editable ? theHiddenClsIcon : theReadonlyHiddenClsIcon);
        }
        appendText(getInstanceCountString(cls));
        //appendText(getOtherString(cls));
    }

    private void loadFacet(Facet facet) {
        Icon icon = (facet.isEditable()) ? theFacetIcon : theReadonlyFacetIcon;
        setMainIcon(icon);
        setMainText(facet.getBrowserText());
    }

    private void loadInstance(Instance instance) {
        setMainText(instance.getBrowserText());
        setMainIcon(instance.isEditable() ? theInstanceIcon : theReadonlyInstanceIcon);
        appendText(getOtherString(instance));
    }

    public void loadNull() {
    }

    private void loadQuery(InstancesQuery query) {
        setMainText(query.getName());
        setMainIcon(theQueryIcon);
    }

    private void loadSlot(Slot slot) {
        Icon icon = (slot.isEditable()) ? theSlotIcon : theReadonlySlotIcon;
        setMainIcon(icon);
        setMainText(slot.getBrowserText());
    }

    public void setDisplayDirectInstanceCount(boolean b) {
        itsInstanceCountType = b ? DIRECT : NONE;
    }

    public void setDisplayFrameTypeIcon(boolean b) {
        displayFrameTypeIcon = b;
    }

    public void setDisplayInstanceCount(boolean b) {
        itsInstanceCountType = b ? ALL : NONE;
    }

    public void setDisplayStringType(boolean b) {
        itsStringType = b ? DIRECT : NONE;
    }

    public void setDisplayTrailingIcons(boolean b) {
        displayAbstractIcon = b;
        displayMultipleParentsIcon = b;
        displayDefaultMetaclassIcon = b;
        hasLoadedIconFlags = true;
    }

    public void setMainIcon(Icon icon) {
        if (displayFrameTypeIcon) {
            super.setMainIcon(icon);
        }
    }
}
