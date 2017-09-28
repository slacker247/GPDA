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
import edu.stanford.smi.protege.model.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class Journal {
    private static String _userName = SystemUtilities.getUserName();
    private static PrintWriter _output;
    private static PrintWriter _defaultOutput = new PrintWriter(System.out);
    private static boolean _isRecording = false;
    private static String _journalFileName;

    private static String collectionToString(Collection c) {
        boolean isFirst = true;
        StringBuffer buffer = new StringBuffer();
        Iterator i = c.iterator();
        while (i.hasNext()) {
            if (isFirst) {
                isFirst = false;
            } else {
                buffer.append(", ");
            }
            buffer.append(objectToString(i.next()));
        }
        return buffer.toString();
    }

    public static void enter(Object source, String command) {
        if (_isRecording) {
            enter(source, command, new String[0], new Object[0]);
        }
    }

    private static void enter(Object source, String command, String[] argnames, Object[] arguments) {
        StringBuffer entry = new StringBuffer();
        entry.append(new Date().toString());
        entry.append(", ");
        if (_userName.trim().length() == 0) {
            _userName = "<unknown user>";
        }
        entry.append(_userName);
        entry.append(", ");
        entry.append(command);
        entry.append(", ");
        entry.append(getSourceText(source));
        for (int i = 0; i < arguments.length; ++i) {
            entry.append(", ");
            entry.append(argnames[i]);
            entry.append('=');
            entry.append(objectToString(arguments[i]));
        }
        PrintWriter writer = (_output == null) ? _defaultOutput : _output;
        writer.println(entry);
        writer.flush();
    }

    public static void enter(Object source, String command, String argname1, Object arg1) {
        if (_isRecording) {
            enter(source, command, new String[]{argname1}, new Object[]{arg1});
        }
    }

    public static void enter(Object source, String command, String argname1, Object arg1, String argname2, Object arg2) {
        if (_isRecording) {
            enter(source, command, new String[]{argname1, argname2}, new Object[]{arg1, arg2});
        }
    }

    public static void enter(Object source, String command, String argname1, Object arg1, String argname2, Object arg2, String argname3, Object arg3) {
        if (_isRecording) {
            enter(source, command, new String[]{argname1, argname2, argname3}, new Object[]{arg1, arg2, arg3});
        }
    }

    public static void enter(Object source, String command, String argname1, Object arg1, String argname2, Object arg2,
            String argname3, Object arg3, String argname4, Object arg4) {
        if (_isRecording) {
            enter(source, command, new String[]{argname1, argname2, argname3, argname4}, new Object[]{arg1, arg2, arg3, arg4});
        }
    }

    public static void enter(Object source, String command, String argname1, Object arg1, String argname2, Object arg2,
            String argname3, Object arg3, String argname4, Object arg4, String argname5, Object arg5) {
        if (_isRecording) {
            enter(source, command, new String[]{argname1, argname2, argname3, argname4, argname5},
                    new Object[]{arg1, arg2, arg3, arg4, arg5});
        }
    }

    private static String getClassName(Object object) {
        String className;
        if (object instanceof Class) {
            className = ((Class) object).getName();
        } else {
            className = object.getClass().getName();
        }
        int index = className.lastIndexOf('.');
        className = className.substring(index + 1);
        return className;
    }

    public static String getJournalFile() {
        return _journalFileName;
    }

    private static String getSourceText(Object source) {
        String sourceText;
        if (source instanceof KnowledgeBase) {
            sourceText = "kb=" + ((KnowledgeBase) source).getName();
        } else if (source instanceof Frame) {
            sourceText = "kb=" + ((Frame) source).getKnowledgeBase().getName();
        } else if (source instanceof String) {
            sourceText = "source=" + source.toString();
        } else {
            sourceText = "source=" + source.getClass().getName();
        }
        return sourceText;
    }

    public static String getUserName() {
        return _userName;
    }

    private static String objectToString(Object o) {
        String string;
        if (o == null) {
            string = "<null>";
        } else if (o instanceof Collection) {
            string = "{" + collectionToString((Collection) o) + "}";
        } else if (o instanceof Frame) {
            string = ((Frame)o).getName();
        } else {
            string = o.toString();
        }
        return string;
    }

    public static void setJournalFile(String fileName) {
        try {
            _output = new PrintWriter(FileUtilities.getWriter(fileName, true));
            _journalFileName = fileName;
        } catch (RuntimeException e) {
            Log.exception(e, Journal.class, "setJournalFile", fileName);
            _journalFileName = null;
        }
    }

    public static void setUserName(String userName) {
        _userName = userName;
    }

    public static void startRecording() {
        if (!_isRecording) {
            enter("Journal", "start recording");
            _isRecording = true;
        }
    }

    public static void stopRecording() {
        if (_isRecording) {
            enter("Journal", "stop recording");
            _output.flush();
            _output.close();
            _isRecording = false;
        }
    }
}
