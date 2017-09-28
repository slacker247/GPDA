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


import java.util.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public abstract class AbstractEvent extends EventObject {
    private int _eventType;
    private Object _argument1;
    private Object _argument2;
    private Object _argument3;

    public AbstractEvent(Object source, int type) {
        this(source, type, null, null, null);
    }

    public AbstractEvent(Object source, int type, Object arg1) {
        this(source, type, arg1, null, null);
    }

    public AbstractEvent(Object source, int type, Object arg1, Object arg2) {
        this(source, type, arg1, arg2, null);
    }

    public AbstractEvent(Object source, int type, Object arg1, Object arg2, Object arg3) {
        super(source);
        _eventType = type;
        _argument1 = arg1;
        _argument2 = arg2;
        _argument3 = arg3;
    }

    public Object getArgument() {
        return getArgument1();
    }

    public Object getArgument1() {
        return _argument1;
    }

    public Object getArgument2() {
        return _argument2;
    }

    public int getEventType() {
        return _eventType;
    }
}
