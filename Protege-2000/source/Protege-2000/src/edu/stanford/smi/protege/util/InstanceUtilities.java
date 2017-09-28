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
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.model.Frame;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class InstanceUtilities {
    private final static String CLASS_RECTANGLE = "Rectangle";
    private final static String CLASS_DIMENSION = "Dimension";

    private final static String SLOT_X = "x";
    private final static String SLOT_Y = "y";
    private final static String SLOT_WIDTH = "width";
    private final static String SLOT_HEIGHT = "height";

    public static Dimension getDimension(Instance instance) {
        Assert.assertEquals("class name", instance.getDirectType().getName(), CLASS_DIMENSION);
        Dimension d = new Dimension();
        d.width = getInt(instance, SLOT_WIDTH);
        d.height = getInt(instance, SLOT_HEIGHT);
        return d;
    }

    private static int getInt(Instance instance, String name) {
        Object o = ModelUtilities.getOwnSlotValue(instance, name);
        Integer i;
        if (o instanceof String) {
            i = new Integer((String) o);
        } else {
            i = (Integer) o;
        }
        // Integer i = (Integer) ModelUtilities.getOwnSlotValue(instance, name);
        return (i == null) ? 0 : i.intValue();
    }

    public static Rectangle getRectangle(Instance instance) {
        Rectangle r = new Rectangle();
        r.x = getInt(instance, SLOT_X);
        r.y = getInt(instance, SLOT_Y);
        r.width = getInt(instance, SLOT_WIDTH);
        r.height = getInt(instance, SLOT_HEIGHT);
        return r;
    }

    public static void setDimension(Instance instance, Dimension d) {
        Assert.assertEquals("class name", instance.getDirectType().getName(), CLASS_DIMENSION);
        setInt(instance, SLOT_WIDTH, d.width);
        setInt(instance, SLOT_HEIGHT, d.height);
    }

    private static void setInt(Instance instance, String name, int i) {
        ModelUtilities.setOwnSlotValue(instance, name, new Integer(i));
    }

    public static void setRectangle(Instance instance, Rectangle r) {
        setInt(instance, SLOT_X, r.x);
        setInt(instance, SLOT_Y, r.y);
        setInt(instance, SLOT_WIDTH, r.width);
        setInt(instance, SLOT_HEIGHT, r.height);
    }
}
