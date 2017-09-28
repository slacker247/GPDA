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
import javax.swing.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public abstract class AllowableAction extends AbstractAction {
    private Selectable _selectable;
    private boolean _isAllowed = true;

    public AllowableAction(String name, String description, Icon icon, Selectable selectable) {
        super(name, icon);
        putValue(Action.SHORT_DESCRIPTION, description);
        _selectable = selectable;
        if (_selectable != null) {
            _selectable.addSelectionListener(
                new SelectionListener() {
                    public void selectionChanged(SelectionEvent event) {
                        onSelectionChange();
                        updateEnabledFlag();
                    }
                }
            );
            setEnabled(false);
        }
    }

    public AllowableAction(String name, Icon icon, Selectable selectable) {
        this(name, name, icon, selectable);
    }

    public Selectable getSelectable() {
        return _selectable;
    }

    public Collection getSelection() {
        return (_selectable == null) ? Collections.EMPTY_LIST : _selectable.getSelection();
    }

    private boolean hasSelection() {
        return _selectable != null && !getSelection().isEmpty();
    }

    public void onSelectionChange() {
        // do nothing
    }

    public void setAllowed(boolean b) {
        _isAllowed = b;
        updateEnabledFlag();
    }

    private void updateEnabledFlag() {
        setEnabled(_isAllowed && (_selectable == null || hasSelection()));
    }
}
