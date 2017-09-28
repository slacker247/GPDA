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


import java.io.*;
import java.util.*;

/**
 *  Default Logger implementation.  This implementation writes messages to System.err.
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class DefaultLogger implements Log.Logger {
    private final static boolean INDENTATION_ENABLED = false;
    private int _nEntries;
    private PrintWriter _writer;
    private boolean _displayTrace = true;
    private boolean _displayWarnings = true;
    private boolean _doPause = false;
    private boolean _showStack = false;

    private int _directTraceCount;
    private int _directWarningCount;
    private int _directErrorCount;

    public DefaultLogger() {
        _writer = new PrintWriter(System.err);
    }

    public DefaultLogger(Writer writer) {
        _writer = new PrintWriter(writer);
    }

    private String createMessage(String prefix, String entry, Object object, String methodName, Object[] arguments) {
        StringBuffer buf = new StringBuffer();
        if (INDENTATION_ENABLED) {
            for (int i = 0; i < _nEntries; ++i) {
                buf.append("    ");
            }
        }
        buf.append(prefix);
        buf.append(": ");
        buf.append(getClassName(object));
        buf.append(".");
        buf.append(methodName);
        buf.append("(");
        for (int i = 0; i < arguments.length; ++i) {
            if (i != 0) {
                buf.append(", ");
            }
            buf.append(toString(arguments[i]));
        }
        buf.append(")");
        buf.append(" ");
        buf.append(entry);
        if (!(object instanceof Class)) {
            buf.append(" - ");
            buf.append(object);
            buf.append(" ");
            buf.append(Integer.toHexString(object.hashCode()).toUpperCase());
        }
        return buf.toString();
    }

    public void enter(Object object, String methodName, Object[] arguments) {
        trace("enter", object, methodName, arguments);
        ++_nEntries;
    }

    public void error(String entry, Object object, String methodName, Object[] arguments) {
        output("ERROR", entry, object, methodName, arguments);
        _directErrorCount++;
    }

    public void exception(Throwable t, Object object, String methodName, Object[] arguments) {
        output("Exception", "failed", object, methodName, arguments);
        t.printStackTrace(_writer);
        _writer.flush();
        _directErrorCount++;
    }

    public void exit(Object object, String methodName, Object[] arguments) {
        trace("exit", object, methodName, arguments);
        --_nEntries;
    }

    private String getClassName(Object o) {
        String className;
        if (o instanceof Class) {
            className = ((Class) o).getName();
        } else {
            className = o.getClass().getName();
        }
        int index = className.lastIndexOf('.');
        return className.substring(index + 1);
    }

    public int getErrorCount() {
        return _directErrorCount;
    }

    private String getMethodText(String methodName, Object[] arguments) {
        StringBuffer buf = new StringBuffer();
        return buf.toString();
    }

    public int getTraceCount() {
        return _directTraceCount + getWarningCount();
    }

    public int getWarningCount() {
        return _directWarningCount + getErrorCount();
    }

    private void output(String prefix, String entry, Object object, String methodName, Object[] arguments) {
        output(prefix, entry, object, methodName, arguments, false);
    }

    private void output(String prefix, String entry, Object object, String methodName, Object[] arguments, boolean stack) {
        String message = createMessage(prefix, entry, object, methodName, arguments);
        if (stack || _showStack) {
            new Throwable(message).printStackTrace(_writer);
        } else {
            _writer.println(message);
        }
        _writer.flush();
        if (_doPause) {
            SystemUtilities.pause();
        }
    }

    public void resetCounts() {
        _directTraceCount = 0;
        _directWarningCount = 0;
        _directErrorCount = 0;
    }

    public void setDisplayTrace(boolean b) {
        _displayTrace = b;
    }

    public void setDisplayWarnings(boolean b) {
        _displayWarnings = b;
    }

    public void setDoPause(boolean b) {
        _doPause = b;
    }

    public void setShowStack(boolean b) {
        _showStack = b;
    }

    public void stack(String entry, Object object, String method, Object[] arguments) {
        output("Stack Dump", entry, object, method, arguments, true);
        _directTraceCount++;
    }

    private String toString(Object o) {
        String string;
        try {
            if (o == null) {
                string = null;
            } else if (o instanceof Collection) {
                string = "[" + CollectionUtilities.toString((Collection) o) + "]";
            } else if (o.getClass().isArray()) {
                string = "{" + CollectionUtilities.toString(Arrays.asList((Object[]) o)) + "}";
            } else {
                string = o.toString();
            }
        } catch (Exception e) {
            string = "<<toString() exception>>";
        }
        return string;
    }

    public void trace(String entry, Object object, String methodName, Object[] arguments) {
        if (_displayTrace) {
            output("Trace", entry, object, methodName, arguments);
        }
        _directTraceCount++;
    }

    public void warning(String entry, Object object, String methodName, Object[] arguments) {
        if (_displayWarnings || _displayTrace) {
            output("Warning", entry, object, methodName, arguments);
        }
        _directWarningCount++;
    }
}
