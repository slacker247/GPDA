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

package edu.stanford.smi.protege.util;


import java.awt.*;
import javax.swing.*;

/**
 * Layout manager that simply scales the existing components by default.  A component
 * can also be designated to take up all of either the extra horizontal or vertical space.
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class ResizingLayout implements LayoutManager2 {
    public final static String HORIZONTAL_STRETCHER = "horizonal_stretcher";
    public final static String VERTICAL_STRETCHER = "vertical_strecher";
    private Dimension _previousSize;

    public ResizingLayout() {
    }

    public void addLayoutComponent(Component c, Object constraint) {
        // do nothing
    }

    public void addLayoutComponent(String s, Component c) {
        // do nothing
    }

    private Component getHorizontalStretcher(Container container) {
        return (Component) ((JComponent) container).getClientProperty(HORIZONTAL_STRETCHER);
    }

    public float getLayoutAlignmentX(Container c) {
        return 0.5f;
    }

    public float getLayoutAlignmentY(Container c) {
        return 0.5f;
    }

    public static Dimension getSize(Container container) {
        Dimension size = new Dimension();
        for (int i = 0; i < container.getComponentCount(); ++i) {
            Rectangle r = container.getComponent(i).getBounds();
            size.width = Math.max(size.width, r.x + r.width);
            size.height = Math.max(size.height, r.y + r.height);
        }
        return size;
    }

    private Component getVerticalStretcher(Container container) {
        return (Component) ((JComponent) container).getClientProperty(VERTICAL_STRETCHER);
    }

    public void invalidateLayout(Container c) {
        // do nothing
    }

    public void layoutContainer(Container container) {
        Component verticalStretcher = getVerticalStretcher(container);
        Component horizontalStretcher = getHorizontalStretcher(container);
        Dimension newSize = container.getSize();
        Dimension oldSize = (_previousSize == null) ? preferredLayoutSize(container) : _previousSize;
        if (!newSize.equals(oldSize)) {
            Point slidePoint = new Point();
            if (horizontalStretcher != null) {
                slidePoint.x = horizontalStretcher.getX() + horizontalStretcher.getWidth();
            }
            if (verticalStretcher != null) {
                slidePoint.y = verticalStretcher.getY() + verticalStretcher.getHeight();
            }
            for (int i = 0; i < container.getComponentCount(); ++i) {
                Component c = container.getComponent(i);
                resize(c, oldSize, newSize, slidePoint, horizontalStretcher, verticalStretcher);
            }
        }
        _previousSize = newSize;
    }

    public Dimension maximumLayoutSize(Container c) {
        return new Dimension(10000, 10000);
    }

    public Dimension minimumLayoutSize(Container c) {
        return new Dimension();
    }

    public Dimension preferredLayoutSize(Container container) {
        return getSize(container);
    }

    public void removeLayoutComponent(Component c) {
        // do nothing
    }

    private void resize(Component c, Dimension oldContainerSize, Dimension newContainerSize, Point slidePoint, Component horizontalStretcher, Component verticalStretcher) {
        Rectangle r = c.getBounds();
        if (horizontalStretcher == null) {
            r.x = (int) Math.round(((double) r.x * newContainerSize.width) / oldContainerSize.width);
            r.width = (int) Math.round(((double) r.width * newContainerSize.width) / oldContainerSize.width);
        } else if (c == horizontalStretcher) {
            r.width += newContainerSize.width - oldContainerSize.width;
        } else if (r.x >= slidePoint.x) {
            r.x += newContainerSize.width - oldContainerSize.width;
        } else {
            // do nothing
        }
        if (verticalStretcher == null) {
            r.y = (int) Math.round(((double) r.y * newContainerSize.height) / oldContainerSize.height);
            r.height = (int) Math.round(((double) r.height * newContainerSize.height) / oldContainerSize.height);
        } else if (c == verticalStretcher) {
            r.height += newContainerSize.height - oldContainerSize.height;
        } else if (r.y >= slidePoint.y) {
            r.y += newContainerSize.height - oldContainerSize.height;
        } else {
            // do nothing
        }
        c.setBounds(r);
    }
}
