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


import java.awt.datatransfer.*;
import java.util.*;
import javax.swing.tree.*;
import edu.stanford.smi.protege.model.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class TransferableCollection implements Transferable {
    private static DataFlavor[] _flavors;
    private Collection _frames;

    static {
        _flavors = new DataFlavor[1];
        try {
            _flavors[0] = new DataFlavor(DataFlavor.javaJVMLocalObjectMimeType);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public TransferableCollection(Collection frames) {
        _frames = new ArrayList(frames);
    }

    public static DataFlavor getFlavor() {
        return _flavors[0];
    }

    public Object getTransferData(DataFlavor f) {
        return _frames;
    }

    public DataFlavor[] getTransferDataFlavors() {
        return _flavors;
    }

    public boolean isDataFlavorSupported(DataFlavor flavor) {
        return getFlavor().equals(flavor);
    }
}
