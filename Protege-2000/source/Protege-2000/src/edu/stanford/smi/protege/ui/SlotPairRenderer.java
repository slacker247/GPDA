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
public class SlotPairRenderer extends DefaultRenderer implements Cloneable {
    private static Icon _directIcon = Icons.getDirectSlotIcon();
    private static Icon _directOverriddenIcon = Icons.getDirectOverriddenSlotIcon();
    private static Icon _overriddenIcon = Icons.getOverriddenSlotIcon();
    private static Icon _inheritedIcon = Icons.getInheritedSlotIcon();
    private static Icon _readonlyDirectIcon = Icons.getReadonlyDirectSlotIcon();
    private static Icon _readonlyInheritedIcon = Icons.getReadonlyInheritedSlotIcon();
    private static Icon _inverseSlotIcon = Icons.getInverseSlotIcon();
    private static SlotPairRenderer _prototypeInstance = new SlotPairRenderer();

    public static SlotPairRenderer createInstance() {
        SlotPairRenderer result;
        try {
            result = (SlotPairRenderer) _prototypeInstance.clone();
        } catch (CloneNotSupportedException e) {
            Log.exception(e, SlotPairRenderer.class, "createInstance");
            result = null;
        }
        return result;
    }

    public void load(Object value) {
        FrameSlotCombination combination = (FrameSlotCombination) value;
        Cls cls = (Cls) combination.getFrame();
        Slot slot = combination.getSlot();
        String text = slot.getBrowserText();
        Icon icon;
        if (cls.hasDirectTemplateSlot(slot)) {
            if (cls.isEditable()) {
                icon = _directIcon;
            } else {
                icon = _readonlyDirectIcon;
            }
        } else if (cls.hasInheritedTemplateSlot(slot)) {
            if (cls.isEditable()) {
                icon = _inheritedIcon;
            } else {
                icon = _readonlyInheritedIcon;
            }
        } else {
            icon = null;
        }
        if (!cls.isEditable()) {
            setGrayedText(true);
        }
        setMainText(text);
        setMainIcon(icon);
        if (cls.hasOverriddenTemplateSlot(slot)) {
            if (cls.hasDirectlyOverriddenTemplateSlot(slot)) {
                appendIcon(_directOverriddenIcon);
            } else {
                appendIcon(_overriddenIcon);
            }
        }
        if (slot.getInverseSlot() != null) {
            appendIcon(_inverseSlotIcon);
        }
    }

    public static void setPrototypeInstance(SlotPairRenderer renderer) {
        _prototypeInstance = renderer;
    }
}
