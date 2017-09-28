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
import java.net.*;
import java.util.*;
import java.util.List;
import javax.swing.*;
import edu.stanford.smi.protege.*;

/**
 * A utility class for working with files.
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class FileUtilities {
    private static URL _baseURL;
    private static List _currentWorkingDirectoryStack = new ArrayList();
    private final static int BUFFER_SIZE = 10000000;	// 10 Meg
    private final static char EXTENSION_SEPARATOR = '.';
    private static final String TEMP_EXTENSION = ".tmp";

    public static File createTempFile(File directory, String name) throws IOException {
        File tmpFile = null;
        if (name != null) {
            File permanentFile = new File(directory, name);
            if (permanentFile.exists() && !permanentFile.canWrite()) {
                throw new IOException("Cannot write to " + permanentFile + ".  Perhaps it is write-protected");
            }
            tmpFile = new File(directory, name + TEMP_EXTENSION);
        }
        return tmpFile;
    }

    public static Writer createWriter(File file) throws IOException {
        return (file == null) ? (Writer) null : new BufferedWriter(new FileWriter(file));
    }

    public static String getAbsoluteDirectory(String file) {
        return new File(getAbsolutePath(file)).getParent();
    }

    public static String getAbsolutePath(String rawfileName) {
        String fileName = new File(rawfileName).getName();
        String path = null;
        if (_baseURL == null) {
            File file = null;
            String cwd = getCurrentWorkingDirectory();
            if (cwd != null) {
                file = new File(cwd, fileName);
                try {
                    path = file.getCanonicalPath();
                } catch (IOException e) {
                    path = file.getAbsolutePath();
                }
            }
        } else {
            try {
                path = new URL(_baseURL, fileName).toString();
            } catch (MalformedURLException e) {
                path = fileName;
            }
        }
        return path;
    }

    public static String getBaseName(String file) {
        String baseName;
        String name = getName(file);
        int index = name.lastIndexOf(EXTENSION_SEPARATOR);
        if (index >= 0) {
            baseName = name.substring(0, index);
        } else {
            baseName = name;
        }
        return baseName;
    }

    public static String getCurrentWorkingDirectory() {
        String cwd;
        int last = _currentWorkingDirectoryStack.size() - 1;
        if (last < 0) {
            cwd = null;
        } else {
            cwd = (String) _currentWorkingDirectoryStack.get(last);
        }
        return cwd;
    }

    public static String getDirectory(String file) {
        File parentFile = new File(file).getParentFile();
        return (parentFile == null) ? (String) null : parentFile.getAbsolutePath();
    }

    public static String getExtension(String file) {
        String extension;
        String name = getName(file);
        int index = name.lastIndexOf(name, EXTENSION_SEPARATOR);
        if (index >= 0) {
            extension = name.substring(index);
        } else {
            extension = "";
        }
        return extension;
    }

    private static Reader getFileReader(String name) {
        Reader reader;
        try {
            String filename = getAbsolutePath(name);
            // Log.trace("file: " + filename, FileUtilities.class, "getFileReader", name);
            Component c = Application.getMainWindow();
            if (c == null) {
                reader = new FileReader(filename);
            } else {
                InputStream is = new FileInputStream(filename);
                is = new ProgressMonitorInputStream(c, "Reading " + name, is);
                reader = new InputStreamReader(is);
            }
            reader = new BufferedReader(reader);
        } catch (FileNotFoundException e) {
            Log.warning("File not found", FileUtilities.class, "getFileReader", name);
            reader = null;
        }
        return reader;
    }

    public static String getName(String file) {
        return new File(file).getName();
    }

    public static Reader getReader(String name) {
        Reader reader;
        if (name == null) {
            reader = null;
        } else if (_baseURL != null) {
            reader = getURLReader(name);
        } else {
            reader = getFileReader(name);
        }
        return reader;
    }

    public static Reader getResourceReader(Class clas, String path) {
        InputStream stream = getResourceStream(clas, path);
        Reader reader;
        if (stream == null) {
            reader = null;
        } else {
            reader = new BufferedReader(new InputStreamReader(stream));
        }
        return reader;
    }

    public static Reader getResourceReader(Class clas, String directory, String name) {
        return getResourceReader(clas, directory + "/" + name);
    }

    public static InputStream getResourceStream(Class clas, String path) {
        Assert.assertNotNull("class", clas);
        Assert.assertNotNull("path", path);
        return clas.getResourceAsStream(path);
    }

    public static InputStream getResourceStream(Class clas, String directory, String name) {
        return getResourceStream(clas, directory + "/" + name);
    }

    private static Reader getURLReader(String name) {
        Reader reader;
        try {
            reader = new InputStreamReader(new URL(_baseURL, name).openStream());
            reader = new BufferedReader(reader, BUFFER_SIZE);
        } catch (Exception e) {
            Log.exception(e, FileUtilities.class, "getURLReader", name);
            reader = null;
        }
        return reader;
    }


    public static Writer getWriter(String name) {
        return getWriter(name, false);
    }

    public static Writer getWriter(String name, boolean append) {
        Writer writer;
        if (name == null) {
            writer = null;
        } else {
            try {
                String fileName = getAbsolutePath(name);
                writer = new FileWriter(fileName, append);
                writer = new BufferedWriter(writer, BUFFER_SIZE);
                // Log.trace("full writer name=" + fileName, FileUtilities.class, "getWriter", name);
            } catch (IOException e) {
                Log.exception(e, SystemUtilities.class, "getWriter", name);
                writer = null;
            }
        }
        return writer;
    }

    private static boolean isURL(String name) {
        return name.startsWith("http:") || name.startsWith("file:");
    }

    public static void makeTempFilePermanent(File tmpFile) throws IOException {
        if (tmpFile != null) {
            String tmpFileName = tmpFile.toString();
            if (tmpFileName.endsWith(TEMP_EXTENSION)) {
                String fileName = tmpFileName.substring(0, tmpFileName.length() - TEMP_EXTENSION.length());
                replaceFile(tmpFile, new File(fileName));
            } else {
                throw new IOException("Not a temporary file: " + tmpFile);
            }
        }
    }

    public static void popCurrentWorkingDirectory() {
        int last = _currentWorkingDirectoryStack.size() - 1;
        _currentWorkingDirectoryStack.remove(last);
    }

    public static void pushCurrentWorkingDirectoryFromFile(String name) {
        String directory = getDirectory(name);
        if (directory == null) {
            directory = getCurrentWorkingDirectory();
        }
        _currentWorkingDirectoryStack.add(directory);
    }

    public static String relativePath(String path) {
        return relativePath(path, getCurrentWorkingDirectory());
    }

    private static String relativePath(String path, String basePath) {
        String absolutePath = getAbsolutePath(path);
        String absoluteBasePath = basePath; // getAbsolutePath(basePath);

        String directory = getDirectory(path);
        String file = getName(path);
        while (directory != null && !absoluteBasePath.startsWith(directory)) {
            directory = getDirectory(directory);
        }

        String relativePath;
        if (directory == null) {
            relativePath = path;
        } else {
            int len = directory.length();
            relativePath = absolutePath.substring(len + 1);
            StringBuffer prefix = new StringBuffer();
            String baseDir = absoluteBasePath;
            // getDirectory(absoluteBasePath);
            while (baseDir != null && !baseDir.equals(directory)) {
                prefix.append(".." + File.separator);
                baseDir = getDirectory(baseDir);
            }
            relativePath = prefix + relativePath;
        }
        return relativePath;
    }

    public static void replaceFile(File tmpFile, File file) throws IOException {
        if (file.exists()) {
            // The File api lets us delete files which are not writable.  This
            // is bad so we test for it.
            if (!file.canWrite()) {
                throw new IOException("Cannot write to file " + file + ".  It may be write-protected.");
            } else if (!file.delete()) {
                throw new IOException("Delete of existing " + file + " failed");
            }
        }
        if (!tmpFile.renameTo(file)) {
            throw new IOException("Rename of " + tmpFile + " to " + file + " failed");
        }
    }

    public static void setBaseURL(URL url) {
        // Log.enter(FileUtilities.class, "setBaseURL", url);
        _baseURL = url;
    }

    private static void setCurrentWorkingDirectory(String name) {
        // Log.enter(FileUtilities.class, "setCurrentWorkingDirectory", name);
        _currentWorkingDirectoryStack.clear();
        if (name != null) {
            // String fullname = getAbsolutePath(name);
            String fullname = new File(name).getAbsolutePath();
            _currentWorkingDirectoryStack.add(fullname);
            // Log.trace("fullname=" + fullname, FileUtilities.class, "setCurrentWorkingDirectory");
        }
    }

    public static void setCurrentWorkingDirectoryFromFile(String name) {
        // Log.enter(FileUtilities.class, "setCurrentWorkingDirectoryFromFile", name);
        String dir;
        if (name == null) {
            setCurrentWorkingDirectory(null);
        } else if (isURL(name)) {
            // do nothing
        } else {
            dir = getDirectory(name);
            setCurrentWorkingDirectory(dir);
        }
    }
}
