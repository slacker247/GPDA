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

// java
import java.io.InputStream;
import java.util.Properties;

// protege
import edu.stanford.smi.protege.util.FileUtilities;

/**
 *
 * @author Ray Fergerson
 * @author Jennifer Vendetti
 */
 public final class Text {
    private static String buildFile = "build.properties";
    private static String directory = "files";
    private static Properties props;
    private static String buildNumber = "Unknown";
    private static String buildVersion = "Unknown";
    private static final String PROGRAM_NAME = "Protégé-2000";
    public static final String PROGRAM_NAME_PROPERTY = "resource.text.program_name";

    static {
        try {
            InputStream stream = FileUtilities.getResourceStream(Text.class, directory, buildFile);
            props = new Properties();
            props.load(stream);
        } catch (java.io.IOException e) {
            e.printStackTrace();
        }
    }

    public static String getBuildInfo() {
        buildNumber = props.getProperty("build.number");
        buildNumber = "Build " + buildNumber;
        return buildNumber;
    }

    public static String getProgramName() {
        return System.getProperty(PROGRAM_NAME_PROPERTY, PROGRAM_NAME);
    }

    public static String getVersion() {
        buildVersion = props.getProperty("build.version");
        return buildVersion;
    }
}
