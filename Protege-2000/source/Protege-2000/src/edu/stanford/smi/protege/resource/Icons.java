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

package edu.stanford.smi.protege.resource;


import java.awt.*;
import java.io.*;
import java.net.*;
import java.util.*;
import javax.swing.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.util.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public final class Icons {
    private static Map _icons = new HashMap();

    public static Icon getAbstractClsIcon() {
        return getImageIcon("Abstract");
    }

    public static Icon getAddIcon() {
        return getImageIcon("Add");
    }

    public static Icon getBackIcon() {
        return getImageIcon("Back");
    }

    public static Icon getBlankIcon() {
        return getImageIcon("Blank");
    }

    public static Icon getBugIcon() {
        return getImageIcon("Bug");
    }

    public static Icon getCancelIcon() {
        return getImageIcon("Cancel");
    }

    public static Icon getCascadeWindowsIcon() {
        return getImageIcon("Cascade");
    }

    public static Icon getCloseAllWindowsIcon() {
        return getImageIcon("CloseAllWindows");
    }

    public static Icon getCloseIcon() {
        return getImageIcon("OK");
    }

    public static Icon getClsesAndInstancesIcon() {
        return getImageIcon("ClassesAndInstances");
    }

    public static Icon getClsesIcon() {
        return getImageIcon("Classes");
    }

    public static Icon getClsIcon() {
        return getImageIcon("Class");
    }

    public static Image getClsImage() {
        return getImageIcon("ClassOpaque").getImage();
    }

    public static Icon getClsMetaClsIcon() {
        return getImageIcon("Metaclass");
    }

    public static Icon getConcreteClsIcon() {
        return getImageIcon("Concrete");
    }

    public static Icon getCopyIcon() {
        return getImageIcon("Copy");
    }

    public static Icon getCreateIcon() {
        return getImageIcon("Create");
    }

    public static Icon getCreateYellowStickyIcon() {
        return getImageIcon("CreateYellow");
    }

    public static Icon getCustomizedFormIcon() {
        return getImageIcon("CustomizedForm");
    }

    public static Icon getCutIcon() {
        return getImageIcon("Cut");
    }

    public static Icon getDefaultIcon() {
        return getImageIcon("Default");
    }

    public static Icon getDeleteIcon() {
        return getImageIcon("Delete");
    }

    public static Icon getDeleteYellowStickyIcon() {
        return getImageIcon("DeleteYellow");
    }

    public static Icon getDirectOverriddenSlotIcon() {
        return getImageIcon("DirectOverriddenSlot");
    }

    public static Icon getDirectSlotIcon() {
        return getImageIcon("SlotDirect");
    }

    public static Icon getDownIcon() {
        return getImageIcon("Down");
    }

    public static Icon getEditIcon() {
        return getImageIcon("Edit");
    }

    public static Icon getFacetIcon() {
        return getImageIcon("Facet");
    }

    public static Image getFacetImage() {
        return getImageIcon("FacetOpaque").getImage();
    }

    public static Icon getFacetsIcon() {
        return getImageIcon("Facets");
    }

    public static Icon getFindIcon() {
        return getImageIcon("Find");
    }

    public static Icon getFindNextIcon() {
        return getImageIcon("FindNext");
    }

    public static Icon getFindPreviousIcon() {
        return getImageIcon("FindPrevious");
    }

    public static Icon getFormIcon() {
        return getImageIcon("Form");
    }

    public static Icon getFormsIcon() {
        return getImageIcon("Forms");
    }

    public static Icon getForwardIcon() {
        return getImageIcon("Forward");
    }

    public static Icon getHiddenClsIcon() {
        return getImageIcon("HiddenClass");
    }

    public static Icon getHomeIcon() {
        return getImageIcon("Home");
    }

    private static ImageIcon getImageIcon(String name) {
        ImageIcon icon = (ImageIcon) _icons.get(name);
        if (icon == null || icon.getIconWidth() == -1) {
            icon = ComponentUtilities.loadImageIcon(Icons.class, "image/" + name + ".gif");
            if (icon == null && !name.equals("Ugly")) {
                icon = getImageIcon("Ugly");
            }
            _icons.put(name, icon);
        }
        return icon;
    }

    public static Icon getInconsistenciesIcon() {
        return getImageIcon("Inconsistencies");
    }

    public static Icon getInconsistencyIcon() {
        return getImageIcon("Inconsistency");
    }

    public static Icon getInheritedSlotIcon() {
        return getImageIcon("SlotInherited");
    }

    public static Icon getInstanceIcon() {
        return getImageIcon("Instance");
    }

    public static Image getInstanceImage() {
        return getImageIcon("InstanceOpaque").getImage();
    }

    public static Icon getInstancesIcon() {
        return getImageIcon("Instances");
    }

    public static Icon getInverseSlotIcon() {
        return getImageIcon("InverseSlot");
    }

    public static Icon getLayoutLikeOtherFormIcon() {
        return getImageIcon("LayoutLikeOtherForm");
    }

    public static Icon getLayoutLikeParentIcon() {
        return getImageIcon("LayoutLikeParent");
    }

    public static Icon getMultipleParentsIcon() {
        return getImageIcon("Multiple");
    }

    public static Icon getMultiValuedSlotIcon() {
        return getImageIcon("CardinalityMultiple");
    }

    public static Icon getNerd16x16Icon() {
        return getImageIcon("Nerd16x16");
    }

    public static Image getNerd16x16Image() {
        return getImageIcon("Nerd16x16").getImage();
    }

    public static Icon getNerd32x32Icon() {
        return getImageIcon("Nerd32x32");
    }

    public static Icon getNewProjectIcon() {
        return getImageIcon("NewProject");
    }

    public static Icon getNoIcon() {
        return getImageIcon("No");
    }

    public static Icon getOkIcon() {
        return getImageIcon("OK");
    }

    public static Icon getOpenProjectIcon() {
        return getImageIcon("OpenProject");
    }

    public static Icon getOverriddenSlotIcon() {
        return getImageIcon("OverriddenSlot");
    }

    public static Icon getPasteIcon() {
        return getImageIcon("Paste");
    }

    public static Icon getQueryIcon() {
        return getImageIcon("Query");
    }

    public static Icon getReadonlyAbstractClsIcon() {
        return getImageIcon("ReadonlyAbstractClass");
    }

    public static Icon getReadonlyClsIcon() {
        return getImageIcon("ReadonlyClass");
    }

    public static Icon getReadonlyDefaultIcon() {
        return getImageIcon("ReadonlyDefault");
    }

    public static Icon getReadonlyDirectSlotIcon() {
        return getImageIcon("ReadonlyDirectSlot");
    }

    public static Icon getReadonlyFacetIcon() {
        return getImageIcon("ReadonlyFacet");
    }

    public static Icon getReadonlyHiddenClsIcon() {
        return getImageIcon("ReadonlyHiddenClass");
    }

    public static Icon getReadonlyInheritedSlotIcon() {
        return getImageIcon("ReadonlyInheritedSlot");
    }

    public static Icon getReadonlyInstanceIcon() {
        return getImageIcon("ReadonlyInstance");
    }

    public static Icon getReadonlyMetaClsIcon() {
        return getImageIcon("ReadonlyMetaClass");
    }

    public static Icon getReadonlyMultipleParentsIcon() {
        return getImageIcon("ReadonlyMultiple");
    }

    public static Icon getRedoIcon() {
        return getImageIcon("Redo");
    }

    public static Icon getRelayoutIcon() {
        return getImageIcon("Relayout");
    }

    public static Icon getRemoveCustomizationsIcon() {
        return getImageIcon("RemoveCustomizations");
    }

    public static Icon getRemoveIcon() {
        return getImageIcon("Remove");
    }

    public static Icon getRemoveSlotOverrideIcon() {
        return getImageIcon("RemoveSlotOverride");
    }

    public static Icon getSaveProjectIcon() {
        return getImageIcon("SaveProject");
    }

    public static Icon getSingleValuedSlotIcon() {
        return getImageIcon("CardinalitySingle");
    }

    public static Icon getSlotIcon() {
        return getImageIcon("Slot");
    }

    public static Image getSlotImage() {
        return getImageIcon("SlotOpaque").getImage();
    }

    public static Icon getSlotsIcon() {
        return getImageIcon("Slots");
    }

    public static Icon getSplashIcon() {
        return getImageIcon("Splash");
    }

    public static Icon getUglyIcon() {
        return getImageIcon("Ugly");
    }

    public static Icon getUndoIcon() {
        return getImageIcon("Undo");
    }

    public static Icon getUpIcon() {
        return getImageIcon("Up");
    }

    public static Icon getViewIcon() {
        return getImageIcon("View");
    }

    public static Icon getViewSlotAtClassIcon() {
        return getImageIcon("ViewSlotAtClass");
    }

    public static Icon getViewTopLevelSlotIcon() {
        return getImageIcon("ViewTopLevelSlot");
    }

    public static Icon getViewYellowStickiesIcon() {
        return getImageIcon("ViewYellow");
    }

    public static Icon getYesIcon() {
        return getImageIcon("Yes");
    }
}
