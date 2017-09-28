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

package edu.stanford.smi.protege.resource;

import java.io.*;
import java.util.jar.*;
import edu.stanford.smi.protege.util.*;

/**
 * <p>Utility class for accessing files in protege.jar</p>
 * @author Ray Fergerson
 * @author Jennifer Vendetti
 */
public class Files {
    private final static String CLSES = "standard_project.pont";
    private final static String INSTANCES = "standard_project.pins";
    private final static String DIRECTORY = "files";
    private final static String MANIFEST = "MANIFEST.MF";

    private static Reader getReader(String name) {
        return FileUtilities.getResourceReader(Files.class, DIRECTORY, name);
    }

    public static Reader getSystemClsesReader() {
        return getReader(CLSES);
    }

    public static Reader getSystemInstancesReader() {
        return getReader(INSTANCES);
    }

    /*
     * Another copy of the JAR manifest is stored in the files directory
     * because it doesn't seem possible to read the real manifest of a JAR
     * from inside of the jar.
     */
    public static Manifest getSystemManifest() {
        Manifest m = null;
        try {
            m = new Manifest(FileUtilities.getResourceStream(Files.class, DIRECTORY, MANIFEST));
        } catch (Exception e) {
            Log.exception(e, Files.class, "getSystemManifest");
        }
        return m;
    }
}
