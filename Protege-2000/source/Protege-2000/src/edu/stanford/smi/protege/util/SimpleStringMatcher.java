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


import edu.stanford.smi.protege.util.*;

/**
 * Description of Type
 *
 * @author Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class SimpleStringMatcher implements StringMatcher {
    private static final int EXACT 			= 0;
    private static final int STARTS_WITH 	= 1;
    private static final int ENDS_WITH 		= 3;
    private static final int ANY 			= 4;
    private int _matchType;
    private String _matchString;

    private static final char WILDCARD = '*';

    public SimpleStringMatcher(String s) {
        char first = s.charAt(0);
        int len = s.length();
        char last = s.charAt(len-1);
        if (first == WILDCARD) {
            if (last == WILDCARD) {
                _matchType = ANY;
                _matchString = s.substring(1, len-1);
            } else {
                _matchType = ENDS_WITH;
                _matchString = s.substring(1, len);
            }
        } else {
            if (last == WILDCARD) {
                _matchType = STARTS_WITH;
                _matchString = s.substring(0, len-1);
            } else {
                _matchType = EXACT;
                _matchString = s;
            }
        }
        _matchString = _matchString.toLowerCase();

    }

    /**
     * isMatch method comment.
     */
    public boolean isMatch(String value) {
        value = value.toLowerCase();
        boolean result;
        switch (_matchType) {
            case EXACT :
                result = value.equals(_matchString);
                break;
            case STARTS_WITH :
                result = value.startsWith(_matchString);
                break;
            case ENDS_WITH :
                result = value.endsWith(_matchString);
                break;
            case ANY :
                result = value.indexOf(_matchString) >= 0;
                break;
            default :
                Assert.fail("bad type: " + _matchType);
                result = false;
        }
        return result;
    }
}
