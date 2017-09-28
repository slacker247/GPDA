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
import java.util.List;
import javax.swing.*;
import javax.swing.border.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.widget.*;
import edu.stanford.smi.protege.util.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class DefaultWidgetLayoutStrategy implements WidgetLayoutStrategy {
    private final static int COLUMN_WIDTH = 250;
    private final static int MAX_COLUMNS = 3;

    private boolean canStretchVertically(Component c) {
        return c.getPreferredSize().height > ComponentUtilities.getStandardRowHeight();
    }

    private void doOneColumnLayout(List components) {
        Collections.sort(components, new SlotWidgetComparer());
        Point p = new Point();
        Dimension previousComponentSize = new Dimension();
        Iterator i = components.iterator();
        while (i.hasNext()) {
            Component c = (Component) i.next();
            Dimension currentComponentSize = c.getPreferredSize();
            if (currentComponentSize.width + p.x > COLUMN_WIDTH) {
                p.x = 0;
                p.y += previousComponentSize.height;
            }
            c.setBounds(new Rectangle(p, c.getPreferredSize()));
            p.x += currentComponentSize.width;
            previousComponentSize = currentComponentSize;
        }
    }

    private void doThreeColumnLayout(List components) {
        // TODO: Not implemented yet
        doTwoColumnLayout(components);
    }

    private void doTwoColumnLayout(List components) {
        if (!components.isEmpty()) {
            doOneColumnLayout(components);
            Component lastComponent = (Component) components.get(components.size() - 1);
            Rectangle lastR = lastComponent.getBounds();
            int largestY = lastR.y + lastR.height;
            int limit = largestY / 2;
            int largestX = 0;
            boolean shifting = false;
            int shiftAmount = 0;
            Iterator i = components.iterator();
            while (i.hasNext()) {
                Component c = (Component) i.next();
                Rectangle r = c.getBounds();
                if (!shifting) {
                    if (r.y >= limit) {
                        shiftAmount = r.y;
                        shifting = true;
                    } else {
                        largestX = Math.max(largestX, r.x + r.width);
                    }
                }
                if (shifting) {
                    c.setLocation(largestX + r.x, r.y - shiftAmount);
                }
            }
            lastR = lastComponent.getBounds();
            if (canStretchVertically(lastComponent)) {
                int height = shiftAmount - lastR.y;
                if (height > 0) {
                    lastComponent.setSize(lastR.width, height);
                }
            }
        }
    }

    private int getPreferredNumberOfColumns(List components) {
        int maxWidth = 0;
        int totalHeight = 0;
        Iterator i = components.iterator();
        while (i.hasNext()) {
            Component c = (Component) i.next();
            Dimension d = c.getPreferredSize();
            maxWidth = Math.max(maxWidth, d.width);
            totalHeight += d.height;
        }

        // no worse than square
        int nColumns = (totalHeight + COLUMN_WIDTH - 1) / COLUMN_WIDTH;

        // no less than narrowest widget
        nColumns = Math.max(nColumns, ((maxWidth - 1) / COLUMN_WIDTH) + 1);

        // no more than MAX_COLUMNS
        nColumns = Math.min(MAX_COLUMNS, nColumns);

        return nColumns;
    }

    public void layout(Container container, int index) {
        // Log.enter(this, "layout", container, new Integer(index));
        List allComponents = new ArrayList(Arrays.asList(container.getComponents()));
        List moveableComponents = allComponents.subList(index, allComponents.size());
        int nColumns = getPreferredNumberOfColumns(moveableComponents);
        switch (nColumns) {
            case 1 :
                doOneColumnLayout(moveableComponents);
                break;
            case 2 :
                doTwoColumnLayout(moveableComponents);
                break;
            case 3 :
            default :
                doThreeColumnLayout(moveableComponents);
                break;
        }
        shiftMovableComponents(allComponents, index);
    }

    private void shiftMovableComponents(List allComponents, int index) {
        int height = 0;
        int nComponents = allComponents.size();
        if (index < nComponents) {
            for (int i = 0; i < index; ++i) {
                Component c = (Component) allComponents.get(i);
                Rectangle r = c.getBounds();
                height = Math.max(height, r.y + r.height);
            }
        }
        if (height > 0) {
            for (int i = index; i < nComponents; ++i) {
                Component c = (Component) allComponents.get(i);
                Point p = c.getLocation();
                p.y += height;
                c.setLocation(p);
            }
        }
    }
}
