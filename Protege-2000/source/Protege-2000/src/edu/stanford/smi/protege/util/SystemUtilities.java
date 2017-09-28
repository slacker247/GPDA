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

import java.lang.reflect.*;
import java.io.*;
import java.net.*;
import java.util.*;
import java.util.jar.*;

import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.resource.*;

/**
 * A set of utilities for accessing the underlying system and for manipulating
 * system level objects.
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class SystemUtilities {
    private static ClassLoader _classLoader;
    private static Collection _slotWidgetNames = new ArrayList();
    private static Collection _tabWidgetNames = new ArrayList();
    private static List _factoryNames = new ArrayList();
    private static Collection _factories;
    private static Map _defaultSlotWidgetNames = new HashMap();
    private static Map _pairToClassesMap = new HashMap();
    private static Collection _manifests = new ArrayList();
    private static Collection _jars = new ArrayList();
    private static Collection _packages;

    private static FilenameFilter _jarFilter = new FilenameFilter() {
        public boolean accept(File dir, String name) {
            return name.endsWith(".jar");
        }
    };

    static {
        Collection files = null;
        try {
            files = getClassPathFiles();
            _classLoader = createClassLoader(files);
            Thread.currentThread().setContextClassLoader(_classLoader);
        } catch (SecurityException e) {
            // expect this for applets
        }
        loadManifests(files);
        processManifests();
        printSystemInfo(System.err);
    }

    private static class DefaultEntry {
        public boolean cardinalityMultiple;
        public String typeName;
        public String allowedClsName;

        public DefaultEntry(String cardinality, String typeName, String allowedClsName) {
            Assert.assertNotNull("cardinality", cardinality);
            Assert.assertNotNull("typeName", typeName);
            this.cardinalityMultiple = cardinality.toLowerCase().equals("multiple");
            this.typeName = typeName;
            this.allowedClsName = allowedClsName;
        }

        public DefaultEntry(boolean cardinality, ValueType type, Cls allowedCls) {
            this.cardinalityMultiple = cardinality;
            this.typeName = type.toString();
            this.allowedClsName = (allowedCls == null) ? (String) null : allowedCls.getName();
        }

        public boolean equals(Object o) {
            DefaultEntry rhs = (DefaultEntry) o;
            boolean isEquals =
                (cardinalityMultiple == rhs.cardinalityMultiple)
                    && typeName.equals(rhs.typeName)
                    && SystemUtilities.equals(allowedClsName, rhs.allowedClsName);
            return isEquals;
        }

        public int hashCode() {
            int hashCode = typeName.hashCode();
            hashCode += cardinalityMultiple ? 0 : 1;
            if (allowedClsName != null) {
                hashCode ^= allowedClsName.hashCode();
            }
            return hashCode;
        }

        public String toString() {
            return "DefaultEntry(" + cardinalityMultiple + ", " + typeName + ", " + allowedClsName + ")";
        }
    }

    private static class KeyValuePair {
        private String itsKey;
        private String itsValue;

        public KeyValuePair(String key, String value) {
            itsKey = key;
            itsValue = value;
        }

        public boolean equals(Object o) {
            boolean equals = false;
            if (o instanceof KeyValuePair) {
                KeyValuePair pair = (KeyValuePair) o;
                equals = itsKey.equals(pair.itsKey) && itsValue.equals(pair.itsValue);
            }
            return equals;
        }

        public int hashCode() {
            return itsKey.hashCode() ^ itsValue.hashCode();
        }
    }

    private static void addManifestClasses(Collection classes, Manifest manifest, String key, String value) {
        Iterator i = manifest.getEntries().keySet().iterator();
        while (i.hasNext()) {
            String attributeName = (String) i.next();
            Attributes attributes = manifest.getAttributes(attributeName);
            String attributeValue = attributes.getValue(key);
            if (value.equalsIgnoreCase(attributeValue)) {
                String className = attributeNameToClassName(attributeName);
                Class cls = forName(className);
                if (cls == null) {
                    Log.trace(
                        "Invalid manifest entry: " + attributeName + ", class not found",
                        SystemUtilities.class,
                        "addManifestClasses");
                } else {
                    classes.add(cls);
                }
            }
        }
    }

    private static String attributeNameToClassName(String attributeName) {
        String className;
        if (attributeName.endsWith(".class")) {
            className = attributeName.substring(0, attributeName.length() - 6);
        } else {
            className = attributeName;
        }
        className = className.replace('/', '.');
        return className;
    }

    public static void close(InputStream stream) {
        try {
            if (stream != null) {
                stream.close();
            }
        } catch (IOException e) {
            Log.exception(e, SystemUtilities.class, "close", stream);
        }
    }

    public static void close(Reader reader) {
        try {
            if (reader != null) {
                reader.close();
            }
        } catch (IOException e) {
            Log.exception(e, SystemUtilities.class, "close", reader);
        }
    }

    public static void close(Writer writer) {
        try {
            if (writer != null) {
                writer.close();
            }
        } catch (IOException e) {
            Log.exception(e, SystemUtilities.class, "close", writer);
        }
    }

    private static ClassLoader createClassLoader(Collection files) {
        Collection urls = new ArrayList();
        Iterator i = files.iterator();
        while (i.hasNext()) {
            File file = (File) i.next();
            try {
                urls.add(file.toURL());
            } catch (Exception e) {
                Log.exception(e, SystemUtilities.class, "createClassLoader");
            }
        }
        URL[] urlArray = (URL[]) urls.toArray(new URL[urls.size()]);
        return new URLClassLoader(urlArray, SystemUtilities.class.getClassLoader());
    }

    public static void debugBreak() {
    }

    public static boolean equals(Object lhs, Object rhs) {
        return (lhs == null) ? rhs == null : lhs.equals(rhs);
    }

    public static void exit() {
        try {
            System.exit(0);
        } catch (SecurityException e) {
            // do nothing
        }
    }

    public static Class forName(String className) {
        Class clas = null;
        try {
            // according to the docs, this "if" is not necessary, but they lie.
            if (_classLoader == null) {
                clas = Class.forName(className);
            } else {
                clas = Class.forName(className, true, _classLoader);
            }
        } catch (ClassNotFoundException e) {
            // do nothing
        } catch (Exception e) {
            Log.exception(e, SystemUtilities.class, "forName", className);
        }
        return clas;
    }

    public static void gc() {
        // Log.enter(SystemUtilities.class, "gc");
        System.gc();
        System.runFinalization();
        System.gc();
    }

    public static Collection getAvailableFactories() {
        if (_factories == null) {
            _factories = new ArrayList();
            Iterator i = _factoryNames.iterator();
            while (i.hasNext()) {
                String name = (String) i.next();
                _factories.add(newInstance(name));
            }
        }
        return _factories;
    }

    public static Collection getAvailableFactoryNames() {
        return Collections.unmodifiableCollection(_factories);
    }

    public static Collection getAvailableSlotWidgetNames() {
        return Collections.unmodifiableCollection(_slotWidgetNames);
    }

    public static Collection getAvailableTabWidgetNames() {
        return Collections.unmodifiableCollection(_tabWidgetNames);
    }

    /**
     *  Does a search of the available manifests entries for the specified
     *  attribute key and does a case insensitive match on the specified
     *  attribute value. If there is a match the associated java Class object is
     *  loaded.
     */
    public static Collection getClassesWithAttribute(String key, String value) {
        KeyValuePair pair = new KeyValuePair(key, value);
        Collection c = (Collection) _pairToClassesMap.get(pair);
        if (c == null) {
            c = getManifestClasses(key, value);
            _pairToClassesMap.put(pair, c);
        }
        return c;
    }

    private static Collection getClassPathFiles() {
        Collection files = new ArrayList();
        String dirPath = ApplicationProperties.getApplicationDirectory();
        if (dirPath == null) {
            Log.warning("Unable to find plugins directory", SystemUtilities.class, "getClassPathFiles");
        } else {
            File directory = new File(dirPath, "plugins");
            if (directory.exists()) {
                files.add(directory);
                File[] fileArray = directory.listFiles(_jarFilter);
                Arrays.sort(fileArray);
                files.addAll(Arrays.asList(fileArray));
            }
        }
        return files;
    }

    public static String getDefaultWidgetClassName(boolean cardinality, ValueType type, Cls allowedCls) {
        DefaultEntry entry = new DefaultEntry(cardinality, type, allowedCls);
        String name = (String) _defaultSlotWidgetNames.get(entry);
        if (name == null && type.equals(ValueType.INSTANCE) && allowedCls != null) {
            entry.allowedClsName = null;
            name = (String) _defaultSlotWidgetNames.get(entry);
        }
        //Log.trace("returned name=" + name, SystemUtilities.class, "getDefaultWidgetClassName",
        //        new Boolean(cardinality), type, allowedCls);
        return name;
    }

    private static String getLaunchString(String document) {
        String launchString;
        if (isWinNT() || isWin2000()) {
            launchString = "cmd /c start " + document;
        } else if (isWin9X()) {
            launchString = "start " + document;
        } else {
            String urlString;
            try {
                urlString = new File(document).toURL().toString();
            } catch (MalformedURLException e) {
                Log.exception(e, SystemUtilities.class, "getLaunchString", document);
                // urlString = document;
                urlString = null;
            }
            launchString = "netscape " + urlString;
        }
        return launchString;
    }

    private static Collection getManifestClasses(String key, String value) {
        // Log.enter(SystemUtilities.class, "getManifestClasses", key, value);
        Collection classes = new ArrayList();
        Iterator i = _manifests.iterator();
        while (i.hasNext()) {
            Manifest manifest = (Manifest) i.next();
            addManifestClasses(classes, manifest, key, value);
        }
        return classes;
    }

    private static Collection getManifests() {
        return _manifests;
    }

    public static String getSystemProperty(String property) {
        String value;
        try {
            value = System.getProperty(property);
        } catch (SecurityException e) {
            value = null;
        }
        return value;
    }

    public static String getUserName() {
        return getSystemProperty("user.name");
    }

    public static boolean hasAvailablePackage(String name) {
        boolean result = false;
        if (_packages == null) {
            loadPackages();
        }
        return _packages.contains(name);
    }

    public static void init() {
        // do nothing,  this forces the static initializer to run
    }

    public static boolean isJDK13() {
        String version = SystemUtilities.getSystemProperty("java.vm.version");
        // Log.trace("version=" + version, SystemUtilities.class, "isJDK12");
        return version.startsWith("1.3");
    }

    public static boolean isLoadableClass(String className) {
        return forName(className) != null;
    }

    private static boolean isSet(Attributes attributes, String name) {
        boolean isSet = false;
        String s = attributes.getValue(name);
        if (s != null) {
            isSet = s.toLowerCase().equals("true");
        }
        return isSet;
    }

    private static boolean isWin2000() {
        return getSystemProperty("os.name").indexOf("Windows 2000") != -1;
    }

    private static boolean isWin95() {
        return getSystemProperty("os.name").indexOf("Windows 95") != -1;
    }

    private static boolean isWin98() {
        return getSystemProperty("os.name").indexOf("Windows 98") != -1;
    }

    private static boolean isWin9X() {
        return isWin95() || isWin98();
    }

    private static boolean isWindows() {
        return isWinNT() || isWin95() || isWin98() || isWin2000();
    }

    private static boolean isWinNT() {
        return getSystemProperty("os.name").indexOf("Windows NT") != -1;
    }

    private static void loadDirectoryManifest(File file) {
        try {
            File manifestFile = new File(new File(file, "meta-inf"), "manifest.mf");
            _manifests.add(new Manifest(new FileInputStream(manifestFile)));
        } catch (Exception e) {
            Log.warning("Unable to load", SystemUtilities.class, "loadDirectoryManifest", file);
        }
    }

    private static void loadExtraManifest() {
        String fileName = ApplicationProperties.getExtraManifestPath();
        if (fileName != null) {
            try {
                FileInputStream is = new FileInputStream(fileName);
                Manifest manifest = new Manifest(is);
                _manifests.add(manifest);
            } catch (IOException e) {
                Log.exception(e, SystemUtilities.class, "loadExtraManifest");
            }
        }
    }

    private static void loadJarManifest(File file) {
        try {
            JarFile jar = new JarFile(file);
            _jars.add(jar);
            _manifests.add(jar.getManifest());
        } catch (Exception e) {
            Log.exception(e, SystemUtilities.class, "loadJarManifest", file);
        }
    }

    private static void loadManifests(Collection files) {
        loadSystemManifest();
        loadExtraManifest();
        if (files != null) {
            Iterator i = files.iterator();
            while (i.hasNext()) {
                File file = (File) i.next();
                if (file.isFile()) {
                    loadJarManifest(file);
                } else if (file.isDirectory()) {
                    loadDirectoryManifest(file);
                }
            }
        }
    }

    private static void loadPackages() {
        _packages = new HashSet();
        Iterator jarIter = _jars.iterator();
        while (jarIter.hasNext()) {
            JarFile jar = (JarFile) jarIter.next();
            Enumeration e = jar.entries();
            while (e.hasMoreElements()) {
                JarEntry entry = (JarEntry) e.nextElement();
                if (entry.isDirectory()) {
                    String name = entry.getName();
                    name = name.replace('/', '.');
                    if (name.endsWith(".")) {
                        name = name.substring(0, name.length() - 1);
                    }
                    boolean changed = _packages.add(name);
                    if (changed) {
                        // Log.trace("found package: " + name, SystemUtilities.class, "loadPackages");
                    }
                }
            }
        }
    }

    private static void loadSystemManifest() {
        Manifest manifest = Files.getSystemManifest();
        if (manifest == null) {
            Log.error("Unable to load system manifest", SystemUtilities.class, "loadSystemManifest");
        } else {
            _manifests.add(manifest);
        }
    }

    public static boolean modalDialogInDropWorks() {
        return isJDK13();
    }

    public static Object newInstance(Class clas, Class[] argumentClasses, Object[] arguments) {
        Object instance = null;
        try {
            Constructor constructor = clas.getConstructor(argumentClasses);
            instance = constructor.newInstance(arguments);
        } catch (Exception e) {
            Log.exception(e, SystemUtilities.class, "newInstance", clas, argumentClasses, arguments);
        }
        return instance;
    }

    public static Object newInstance(String className) {
        Object o = null;
        try {
            Class clas = forName(className);
            if (clas == null) {
                Log.warning("no such class", SystemUtilities.class, "newInstance", className);
            } else {
                o = clas.newInstance();
            }
        } catch (Exception e) {
            Log.exception(e, SystemUtilities.class, "newInstance", className);
        }
        return o;
    }

    public static void pause() {
        try {
            System.out.flush();
            System.err.flush();
            System.out.print("Press <Enter> to continue");
            System.in.read();
            while (System.in.available() != 0) {
                System.in.read();
            }
        } catch (Exception e) {
        }
    }

    public static void printMemoryUsage() {
        gc();
        long total = Runtime.getRuntime().totalMemory();
        long free = Runtime.getRuntime().freeMemory();
        String s = "memory: total=" + total + ", used=" + (total - free);
        Log.trace(s, SystemUtilities.class, "printMemoryUsage");
    }

    public static void printSystemInfo(PrintStream stream) {
        stream.println("Protege-2000  version " + Text.getVersion() + ", " + Text.getBuildInfo());
        String name = SystemUtilities.getSystemProperty("java.vm.name");
        String version = SystemUtilities.getSystemProperty("java.vm.version");
        String info = SystemUtilities.getSystemProperty("java.vm.info");
        stream.println("JVM: " + name + " - " + version + ", " + info);

        URL[] urls = (_classLoader instanceof URLClassLoader) ? ((URLClassLoader) _classLoader).getURLs() : null;
        if (urls != null) {
            stream.println("Plugin classpath:");
            for (int i = 0; i < urls.length; ++i) {
                URL url = urls[i];
                stream.print("    ");
                stream.println(url);
            }
        }
        stream.println();
    }

    private static void processManifest(Manifest manifest) {
        // Log.enter(SystemUtilities.class, "processManifest", manifest);
        Iterator i = manifest.getEntries().keySet().iterator();
        while (i.hasNext()) {
            String attributeName = (String) i.next();
            Attributes attributes = manifest.getAttributes(attributeName);
            String className = attributeNameToClassName(attributeName);
            // Log.trace("entry=" + attributeName, SystemUtilities.class, "processManifest", manifest);
            if (isSet(attributes, "Tab-Widget")) {
                // Log.trace("tab widget=" + className, SystemUtilities.class, "processManifest");
                if (isLoadableClass(className)) {
                    _tabWidgetNames.add(className);
                } else {
                    Log.warning("Unable to load manifest tab: " + className, SystemUtilities.class, "processManifest");
                }
            }
            if (isSet(attributes, "Slot-Widget")) {
                // Log.trace("slot widget=" + className, SystemUtilities.class, "processManifest");
                if (isLoadableClass(className)) {
                    _slotWidgetNames.add(className);
                    if (isSet(attributes, "Default-Widget")) {
                        recordDefaults(className, attributes);
                    }
                } else {
                    Log.warning("Unable to load manifest slot widget: " + className, SystemUtilities.class, "processManifest");
                }
            }
            if (isSet(attributes, "Storage-Factory")) {
                // Log.trace("storage factory=" + className, SystemUtilities.class, "processManifest");
                if (isLoadableClass(className)) {
                    int index = _factoryNames.size();
                    if (isSet(attributes, "Default-Factory")) {
                        index = 0;
                    }
                    _factoryNames.add(index, className);
                } else {
                    Log.warning("Unable to load manifest storage factory: " + className, SystemUtilities.class, "processManifest");
                }
            }
        }
    }

    private static void processManifests() {
        Iterator i = _manifests.iterator();
        while (i.hasNext()) {
            Manifest m = (Manifest) i.next();
            processManifest(m);
        }
    }

    private static void recordDefaults(String className, Attributes attributes) {
        String cardinality = attributes.getValue("Default-Widget-For-Cardinality");
        String type = attributes.getValue("Default-Widget-For-Type");
        String allowed_class = attributes.getValue("Default-Widget-For-Allowed-Class");
        DefaultEntry entry = new DefaultEntry(cardinality, type, allowed_class);
        _defaultSlotWidgetNames.put(entry, className);
        // Log.trace("entry=" + entry, SystemUtilities.class, "recordDefaults", className);
    }

    public static void showHTML(String url) {
        String command = getLaunchString(url);
        try {
            Runtime.getRuntime().exec(command);
        } catch (IOException e) {
            Log.exception(e, SystemUtilities.class, "showHTML", url);
        }
    }

    public static void sleepMsec(int msecs) {
        try {
            Thread.sleep(msecs);
        } catch (Exception e) {
            Log.exception(e, SystemUtilities.class, "sleepMsec", new Integer(msecs));
        }
    }

    public static Boolean toBoolean(Object o) {
        Boolean b;
        if (o instanceof Boolean) {
            b = (Boolean) o;
        } else {
            String s = o.toString().toLowerCase();
            if (s.equals("true")) {
                b = Boolean.TRUE;
            } else if (s.equals("false")) {
                b = Boolean.FALSE;
            } else {
                b = null;
            }
        }
        return b;
    }

    public static Float toFloat(Object o) {
        Float f;
        if (o instanceof Float) {
            f = (Float) o;
        } else {
            try {
                f = Float.valueOf(o.toString());
            } catch (Exception e) {
                f = null;
            }
        }
        return f;
    }

    public static Integer toInteger(Object o) {
        Integer i;
        if (o instanceof Integer) {
            i = (Integer) o;
        } else {
            try {
                i = Integer.valueOf(o.toString());
            } catch (Exception e) {
                i = null;
            }
        }
        return i;
    }

    public static ClassLoader getDefaultClassLoader() {
        return _classLoader;
    }
}
