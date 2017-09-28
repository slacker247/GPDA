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

package edu.stanford.smi.RemoteKBTab.toolbox;

import javax.swing.*;
import edu.stanford.smi.RemoteKBTab.*;

/** All AbstractRemoteKBAction classes must have the method setRelationDisplay,
 *  so the action can be associated with the RelationDisplay.  This is somewhat
 *  of a hack, because there is a problem creating the Actions and
 *  RelationDisplay's in the correct order.  They both need pointers to the
 *  other before becoming useful. It seemed that the panel needed the actions
 *  to draw itself first, so the actions are created before the panel and
 *  then their RelationDisplay is set. */
public interface AbstractRemoteKBAction extends Action {
    public void setRelationDisplay(RelationDisplay rd);
}