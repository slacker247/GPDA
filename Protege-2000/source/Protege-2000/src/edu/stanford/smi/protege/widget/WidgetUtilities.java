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

import java.beans.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.model.Frame;
import edu.stanford.smi.protege.util.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class WidgetUtilities {

    public static ClsWidget createClsWidget(WidgetDescriptor descriptor, boolean isDesignTime, Project project, Cls cls) {
        ClsWidget widget;
        try {
            Assert.assertNotNull("descriptor", descriptor);
            Assert.assertNotNull("project", project);
            widget = (ClsWidget) SystemUtilities.newInstance(descriptor.getWidgetClassName());
            if (widget == null) {
                widget = new UglyClsWidget();
            }
            widget.setup(descriptor, isDesignTime, project, cls);
            widget.initialize();
        } catch (Exception e) {
            Log.exception(e, WidgetUtilities.class, "createSlotWidget", descriptor.getWidgetClassName());
            widget = new UglyClsWidget();
        }
        return widget;
    }

    public static SlotWidget createSlotWidget(
        WidgetDescriptor descriptor,
        boolean isDesignTime,
        Project project,
        Cls cls,
        Slot slot) {
        SlotWidget widget;
        try {
            Assert.assertNotNull("descriptor", descriptor);
            Assert.assertNotNull("project", project);
            widget = (SlotWidget) SystemUtilities.newInstance(descriptor.getWidgetClassName());
            if (widget == null) {
                widget = new UglySlotWidget();
            }
            setupSlotWidget(widget, descriptor, isDesignTime, project, cls, slot);
        } catch (Exception e) {
            Log.exception(e, WidgetUtilities.class, "createSlotWidget", descriptor.getWidgetClassName());
            widget = new UglySlotWidget();
            setupSlotWidget(widget, descriptor, isDesignTime, project, cls, slot);
        }
        return widget;
    }

    public static TabWidget createTabWidget(WidgetDescriptor descriptor, Project project) {
        TabWidget result;
        try {
            Assert.assertNotNull("descriptor", descriptor);
            Assert.assertNotNull("project", project);
            result = (TabWidget) SystemUtilities.newInstance(descriptor.getWidgetClassName());
            if (result == null) {
                result = new UglyTabWidget();
            }
            result.setup(descriptor, project);
            result.initialize();
        } catch (Exception e) {
            Log.exception(e, WidgetUtilities.class, "createTabWidget", descriptor.getWidgetClassName());
            result = new UglyTabWidget();
            result.setup(descriptor, project);
            result.initialize();
        }
        return result;
    }

    private static void fixBounds(Widget widget) {
        WidgetDescriptor d = widget.getDescriptor();
        if (d.getBounds() == null) {
            JComponent c = (JComponent) widget;
            Rectangle bounds = new Rectangle(new Point(), c.getPreferredSize());
            d.setBounds(bounds);
            c.setBounds(bounds);
        }
    }

    private static void setupComponent(JComponent c, WidgetDescriptor descriptor) {
        Rectangle bounds = descriptor.getBounds();
        if (bounds != null) {
            c.setBounds(bounds);
        }
    }

    private static void setupSlotWidget(
        SlotWidget widget,
        WidgetDescriptor descriptor,
        boolean isDesignTime,
        Project project,
        Cls cls,
        Slot slot) {
        setupComponent((JComponent) widget, descriptor);
        widget.setup(descriptor, isDesignTime, project, cls, slot);
        widget.initialize();
        fixBounds(widget);
    }
}
