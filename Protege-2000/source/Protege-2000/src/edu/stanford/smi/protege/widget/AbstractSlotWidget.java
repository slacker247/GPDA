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
import javax.swing.border.*;
import edu.stanford.smi.protege.event.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.util.*;

public abstract class AbstractSlotWidget extends _AbstractWidget implements SlotWidget {
    private static final int NORMAL_BORDER_SIZE = 5;
    private static final int SELECTED_BORDER_SIZE = 3;
    private static final Border _normalBorder;
    private static final Border _invalidValueBorder;
    private static final Border _selectedBorder;

    private ClsListener _clsListener = new ClsAdapter() {
        public void templateFacetValueChanged(ClsEvent event) {
            if (event.getSlot() == getSlot()) {
                updateBorder(getValues());
            }
        }
    };

    static {
        int n = NORMAL_BORDER_SIZE;
        int s = SELECTED_BORDER_SIZE;
        int d = n - s;

        Border normalBandBorder = BorderFactory.createEmptyBorder(d, d, d, d);
        Border invalidBandBorder = BorderFactory.createMatteBorder(s, s, s, s, Color.red);
        Border selectedBandBorder = BorderFactory.createMatteBorder(s, s, s, s, Color.blue);
        _invalidValueBorder = BorderFactory.createCompoundBorder(normalBandBorder, invalidBandBorder);
        _selectedBorder = BorderFactory.createCompoundBorder(normalBandBorder, selectedBandBorder);
        _normalBorder = BorderFactory.createEmptyBorder(n, n, n, n);
    }

    private DoubleClickListener _doubleClickListener;

    /**
     * AbstractSlotWidget constructor comment.
     */
    public AbstractSlotWidget() {
        setNormalBorder();
    }

    public void dispose() {
        super.dispose();
        Cls cls = getCls();
        // The test is needed because this widget is current hacked to work for tab widgets
        if (cls != null) {
            cls.removeClsListener(_clsListener);
        }
    }

    protected Action getDoubleClickAction() {
        return new AbstractAction() {
            public void actionPerformed(java.awt.event.ActionEvent event) {
                handleDoubleClick();
            }
        };
    }

    public static int getSelectionBorderSize() {
        return SELECTED_BORDER_SIZE;
    }

    protected void handleDoubleClick() {
        Object o = CollectionUtilities.getFirstItem(getSelection());
        if (o != null) {
            if (_doubleClickListener == null) {
                showInstance((Instance) o);
            } else {
                _doubleClickListener.onDoubleClick(o);
            }
        }
    }

    public void setDoubleClickListener(DoubleClickListener listener) {
        _doubleClickListener = listener;
    }

    public void setInvalidValueBorder() {
        setBorder(_invalidValueBorder);
    }

    public void setNormalBorder() {
        setBorder(_normalBorder);
    }

    public void setSelectedBorder() {
        setBorder(_selectedBorder);
    }

    public void setup(final WidgetDescriptor descriptor, boolean isDesignTime, Project project, Cls cls, Slot slot) {
        super.setup(descriptor, isDesignTime, project, cls, slot);
        // The test is needed because this widget is current hacked to work for tab widgets
        if (cls != null) {
            cls.addClsListener(_clsListener);
        }
    }

    protected void updateBorder(Collection values) {
        String text = null;
        Instance instance = getInstance();
        if (instance != null) {
            text = getKnowledgeBase().getInvalidOwnSlotValuesText(instance, getSlot(), values);
        }
        setToolTipText(text);
        if (text == null) {
            setNormalBorder();
        } else {
            setInvalidValueBorder();
        }
        repaint();
    }
}
