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
import javax.swing.tree.*;
import edu.stanford.smi.protege.*;
import edu.stanford.smi.protege.resource.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.model.Frame;
import edu.stanford.smi.protege.widget.*;
import edu.stanford.smi.protege.util.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class FrameRenderer extends DefaultRenderer implements Cloneable {
    protected final static int NONE = 0;
    protected final static int DIRECT = 1;
    protected final static int ALL = 2;

    protected static Icon _clsIcon = Icons.getClsIcon();
    protected static Icon _metaClsIcon = Icons.getClsMetaClsIcon();
    protected static Icon _readonlyClsIcon = Icons.getReadonlyClsIcon();
    protected static Icon _readonlyMetaClsIcon = Icons.getReadonlyMetaClsIcon();
    protected static Icon _abstractClsIcon = Icons.getAbstractClsIcon();
    protected static Icon _readonlyAbstractClsIcon = Icons.getReadonlyAbstractClsIcon();
    protected static Icon _multipleParentsIcon = Icons.getMultipleParentsIcon();
    protected static Icon _readonlyMultipleParentsIcon = Icons.getReadonlyMultipleParentsIcon();
    protected static Icon _hiddenClsIcon = Icons.getHiddenClsIcon();
    protected static Icon _readonlyHiddenClsIcon = Icons.getReadonlyHiddenClsIcon();
    protected static Icon _instanceIcon = Icons.getInstanceIcon();
    protected static Icon _readonlyInstanceIcon = Icons.getReadonlyInstanceIcon();
    protected static Icon _slotIcon = Icons.getSlotIcon();
    protected static Icon _readonlySlotIcon = Icons.getReadonlyDirectSlotIcon();
    protected static Icon _facetIcon = Icons.getFacetIcon();
    protected static Icon _readonlyFacetIcon = Icons.getReadonlyFacetIcon();
    protected static Icon _defaultIcon = Icons.getDefaultIcon();
    protected static Icon _readonlyDefaultIcon = Icons.getReadonlyDefaultIcon();

    protected int _instanceCountType = NONE;
    protected boolean _hasLoadedIconFlags = false;
    protected boolean _displayFrameTypeIcon = true;
    protected boolean _displayAbstractIcon = true;
    protected boolean _displayMultipleParentsIcon = true;
    protected boolean _displayDefaultMetaclassIcon = true;

    protected static FrameRenderer _frameRendererPrototype = new FrameRenderer();

    public FrameRenderer () {
    }

    public static FrameRenderer createInstance() {
        FrameRenderer renderer;
        try {
            renderer = (FrameRenderer) _frameRendererPrototype.clone();
        } catch (CloneNotSupportedException e) {
            renderer = null;
        }
        return renderer;
    }

    protected void ensureIconFlagsLoaded() {
        if (!_hasLoadedIconFlags) {
            Project p = ProjectManager.getProjectManager().getCurrentProject();
            if (p != null) {
                _displayAbstractIcon = p.getDisplayAbstractClassIcon();
                _displayMultipleParentsIcon = p.getDisplayMultiParentClassIcon();
                _hasLoadedIconFlags = true;
            }
        }
    }

    protected String getInstanceCountString(Cls cls) {
        int count;
        switch (_instanceCountType) {
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
                Assert.fail("bad type: " + _instanceCountType);
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

    public void load(Object value) {
        ensureIconFlagsLoaded();

        if (value instanceof Frame) {
            Frame frameValue = (Frame) value;
            if (!(frameValue.isEditable())) {
                setGrayedText(true);
            }
            if (frameValue.getFrameID() == null) {
                setMainIcon(null);
                setMainText("<deleted frame>");
            } else if (frameValue instanceof Cls) {
                loadCls((Cls) value);
            } else if (frameValue instanceof Slot) {
                loadSlot((Slot) value);
            } else if (frameValue instanceof Facet) {
                loadFacet((Facet) value);
            } else if (frameValue instanceof Instance) {
                loadInstance((Instance) value);
            }
        } else {
            setMainIcon(null);
            setMainText(value.toString());
        }
    }

    protected void loadCls(Cls cls) {
        boolean editable = cls.isEditable();
        if (cls.isClsMetaCls()) {
            setMainIcon(editable ? _metaClsIcon : _readonlyMetaClsIcon);
        } else {
            setMainIcon(editable ? _clsIcon : _readonlyClsIcon);
        }
        setMainText(cls.getBrowserText());
        if (_displayAbstractIcon && cls.isAbstract()) {
            appendIcon(editable ? _abstractClsIcon : _readonlyAbstractClsIcon);
        }
        if (_displayMultipleParentsIcon && cls.getDirectSuperclasses().size() > 1) {
            appendIcon(editable ? _multipleParentsIcon : _readonlyMultipleParentsIcon);
        }
        if (_displayDefaultMetaclassIcon && (cls.isDefaultSlotMetaCls() || cls.isDefaultFacetMetaCls())) {
            appendIcon(editable ? _defaultIcon : _readonlyDefaultIcon);
        }
        if (!cls.isVisible()) {
            appendIcon(editable ? _hiddenClsIcon : _readonlyHiddenClsIcon);
        }
        appendText(getInstanceCountString(cls));
    }

    protected void loadFacet(Facet facet) {
        Icon icon = (facet.isEditable()) ? _facetIcon : _readonlyFacetIcon;
        setMainIcon(icon);
        setMainText(facet.getBrowserText());
    }

    protected void loadInstance(Instance instance) {
        setMainText(instance.getBrowserText());
        setMainIcon(instance.isEditable() ? _instanceIcon : _readonlyInstanceIcon);
    }

    public void loadNull() {
    }

    protected void loadSlot(Slot slot) {
        Icon icon = (slot.isEditable()) ? _slotIcon : _readonlySlotIcon;
        setMainIcon(icon);
        setMainText(slot.getBrowserText());
    }

    public void setDisplayDirectInstanceCount(boolean b) {
        _instanceCountType = b ? DIRECT : NONE;
    }

    public void setDisplayFrameTypeIcon(boolean b) {
        _displayFrameTypeIcon = b;
    }

    public void setDisplayInstanceCount(boolean b) {
        _instanceCountType = b ? ALL : NONE;
    }

    public void setDisplayTrailingIcons(boolean b) {
        _displayAbstractIcon = b;
        _displayMultipleParentsIcon = b;
        _displayDefaultMetaclassIcon = b;
        _hasLoadedIconFlags = true;
    }

    public void setMainIcon(Icon icon) {
        if (_displayFrameTypeIcon) {
            super.setMainIcon(icon);
        }
    }

    public static void setPrototypeInstance(FrameRenderer renderer) {
        _frameRendererPrototype = renderer;
    }
}
