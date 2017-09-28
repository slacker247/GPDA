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

import java.awt.*;
import java.io.*;
import java.util.*;

/**
 * Utility class for accessing system properties and properties from the
 * application properties file.
 *
 * @author Ray Fergerson
 * @author Jennifer Vendetti
 */
public class ApplicationProperties {
    public final static String FILE_NAME = "protege.properties";
    public final static String NEXT_FRAME_NUMBER = "next_frame_number";
    public final static String APPLICATION_DIRECTORY = "protege.dir";
    public final static String DEFAULT_DIRECTORY = "user.dir";
    public final static String EXTRA_MANIFEST_PATH = "protege.plugin.manifest";
    public final static String MRU_PROJECTS = "history.projects.reopen";
    public final static String WELCOME_DIALOG = "ui.welcomedialog.show";
    public final static String MAIN_FRAME_RECTANGLE = "mainframe.rectangle";

    private final static File _propertyFile = new File(getApplicationDirectory(), FILE_NAME);
    private final static Properties _properties = new Properties();

    private final static int num_MRUProjects = 10;
    private static java.util.List _mruProjectList = new ArrayList(num_MRUProjects);

    static {
        try {
            InputStream is = new FileInputStream(_propertyFile);
            _properties.load(is);
            is.close();

            // Populate list of most recently used projects.
            String projectNames = _properties.getProperty(MRU_PROJECTS);
            if (projectNames != null) {
                StringTokenizer st = new StringTokenizer(projectNames, ",");
                for (int i=0; ((i<num_MRUProjects) && (st.hasMoreElements())); i++) {
                    String projectName = (String) st.nextElement();
                    File file = new File(projectName);
                    if (file.exists()) {
                        _mruProjectList.add(projectName);
                    }
                }
            } else {
                // If there are none, use some example projects provided
                // with the protege installation.
                char sep = java.io.File.separatorChar;
                projectNames = getApplicationDirectory() + sep + "examples" +
                               sep + "newspaper" + sep + "newspaper.pprj";
                _mruProjectList.add(projectNames);
                _properties.setProperty(MRU_PROJECTS, projectNames);
            }
        } catch (IOException e) {
            // Log.exception(e, ApplicationProperties.class, "<static>");
        } catch (SecurityException e) {
        }
    }

    public static void addProjectToMRUList(String filePath) {
        if ((filePath != null) && (!filePath.equals(""))) {
            // Add project to beginning of the most recently used list.
            _mruProjectList.add(0, filePath);

            // See if this is a duplicate and remove other entry if necessary.
            for (int i = 1; i < _mruProjectList.size(); i++) {
                String entry = (String) _mruProjectList.get(i);
                if (entry.equals(filePath)) {
                    _mruProjectList.remove(i);
                }
            }

            // Trim off the last element if necessary.
            if (_mruProjectList.size() > num_MRUProjects) {
                _mruProjectList.remove(num_MRUProjects);
            }
            saveMRUProjectList();
        }
    }

    public static void flush() {
        try {
            OutputStream os = new FileOutputStream(_propertyFile);
            _properties.store(os, "Protege Properties");
            os.close();
        } catch (IOException e) {
            Log.exception(e, ApplicationProperties.class, "flush");
        } catch (SecurityException e) {
        }
    }

    public static String getApplicationDirectory() {
        String dir = SystemUtilities.getSystemProperty(APPLICATION_DIRECTORY);
        if (dir == null) {
            dir = SystemUtilities.getSystemProperty(DEFAULT_DIRECTORY);
        }
        return dir;
    }

    public static String getExtraManifestPath() {
        String s = SystemUtilities.getSystemProperty(EXTRA_MANIFEST_PATH);
        if (s != null && s.length() > 1 && s.charAt(0) == '"') {
            s = s.substring(1, s.length() - 1);
        }
        return s;
    }

    public static int getInt(String name, int defaultValue) {
        int value;
        String propString = _properties.getProperty(name);
        if (propString == null) {
            value = defaultValue;
        } else {
            value = Integer.parseInt(propString);
        }
        return value;
    }

    public static java.util.List getMRUProjectList() {
        return new ArrayList(_mruProjectList);
    }

    public static int getOldNextFrameNumber() {
        String nextInstanceString = _properties.getProperty(NEXT_FRAME_NUMBER, "0");
        int nextInstance = Integer.parseInt(nextInstanceString);
        // properties.setProperty(NEXT_FRAME_NUMBER, String.valueOf(nextInstance+1));
        return nextInstance;
    }

    private static Rectangle getRectangle(String name) {
        Rectangle rectangle = null;
        String property = _properties.getProperty(name);
        if (property != null) {
            rectangle = parseRectangle(property);
        }
        return rectangle;
    }

    public static String getString(String name, String defaultValue) {
        return _properties.getProperty(name, defaultValue);
    }

    private static Rectangle parseRectangle(String text) {
        int[] numbers = new int[4];
        int index = 0;
        StringTokenizer st = new StringTokenizer(text);
        while (st.hasMoreTokens() && index < numbers.length) {
            String token = st.nextToken();
            numbers[index] = Integer.parseInt(token);
            ++index;
        }
        return new Rectangle(numbers[0], numbers[1], numbers[2], numbers[3]);
    }

    public static void recordMainFrameProperties(Frame mainFrame) {
        saveRectangle(MAIN_FRAME_RECTANGLE, mainFrame.getBounds());
    }

    public static void restoreMainFrameProperties(Frame mainFrame) {
        Rectangle r = getRectangle(MAIN_FRAME_RECTANGLE);
        if (r == null) {
            mainFrame.setSize(new Dimension(800, 600));
            ComponentUtilities.center(mainFrame);
        } else {
            mainFrame.setBounds(r);
        }
    }

    private static void saveMRUProjectList() {
        StringBuffer buf = new StringBuffer();
        int size = _mruProjectList.size();
        for (int i = 0; i < size; i++) {
            buf.append(_mruProjectList.get(i));
            buf.append(",");
        }
        // Get rid of the comma on the end.
        buf.setLength(buf.length() - 1);
        setProperty(MRU_PROJECTS, buf.toString());
    }

    private static void setProperty(String property, String value) {
        _properties.setProperty(property, value);
        flush();
    }

    private static void saveRectangle(String name, Rectangle r) {
        StringBuffer buffer = new StringBuffer();
        buffer.append(String.valueOf(r.x));
        buffer.append(" ");
        buffer.append(String.valueOf(r.y));
        buffer.append(" ");
        buffer.append(String.valueOf(r.width));
        buffer.append(" ");
        buffer.append(String.valueOf(r.height));
        setProperty(name, buffer.toString());
    }

    public static void setInt(String name, int value) {
        setProperty(name, String.valueOf(value));
    }

    public static void setString(String name, String value) {
        setProperty(name, value);
    }

    public static boolean getWelcomeDialogShow() {
        boolean b;
        String s = getString(WELCOME_DIALOG, "true");
        if (s.compareToIgnoreCase("true") == 0) {
            b = true;
        } else { b = false; }
        return b;
    }

    public static void setWelcomeDialogShow(Boolean b) {
        setString(WELCOME_DIALOG, b.toString());
    }
}
