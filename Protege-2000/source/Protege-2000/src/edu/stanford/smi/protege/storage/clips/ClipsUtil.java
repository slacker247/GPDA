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

package edu.stanford.smi.protege.storage.clips;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class ClipsUtil {

    public final static String TOP_LEVEL_SLOT_CLASS = ":CLIPS_TOP_LEVEL_SLOT_CLASS";
    public final static String FALSE = "FALSE";
    public final static String TRUE = "TRUE";

    private static char REAL_SPACE = ' ';
    private static char FAKE_SPACE = '^';

    private static String replace(String s, char inchar, char outchar) {
        String newString;
        int index = s.indexOf(inchar);
        if (index == -1) {
            newString = s;
        } else {
            StringBuffer buffer = new StringBuffer(s);
            for (int i = index; i != -1; i = s.indexOf(inchar, i + 1)) {
                buffer.setCharAt(i, outchar);
            }
            newString = buffer.toString();
        }
        return newString.intern();
    }

    public static String toExternalString(String s) {
        StringBuffer buffer = new StringBuffer();
        buffer.append('"');
        for (int i = 0; i < s.length(); ++i) {
            char c = s.charAt(i);
            switch (c) {
                case '\n':
                    buffer.append("\\n");
                    break;
                case '\r':
                    // discard
                   break;
                case '\"':
                    buffer.append("\\\"");
                    break;
                case '\\':
                    buffer.append("\\\\");
                    break;
                default:
                    buffer.append(c);
                    break;
            }
        }
        buffer.append('"');
        return buffer.toString();
    }

    public static String toExternalSymbol(String s) {
        return replace(s, REAL_SPACE, FAKE_SPACE);
    }

    public static String toInternalString(String s) {
        String newString;
        if (s.charAt(0) == '"') {
            int len = s.length();
            int bufferLen = 0;
            char[] buffer = new char[len];
            for (int i = 1; i < len - 1; ++i) {
                char c = s.charAt(i);
                if (c == '\\') {
                    ++i;
                    c = s.charAt(i);
                    if (c == 'n') {
                        c = '\n';
                    } else if (c == 'r') {
                        // simply discard '\r'
                        continue;
                    }
                }
                buffer[bufferLen] = c;
                ++bufferLen;
            }
            newString = new String(buffer, 0, bufferLen);
        } else {
            newString = s;
        }
        return newString.intern();
    }

    public static String toInternalSymbol(String s) {
        return replace(s, FAKE_SPACE, REAL_SPACE).intern();
    }
}
