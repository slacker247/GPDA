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

package edu.stanford.smi.protege;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.*;
import javax.swing.*;
import edu.stanford.smi.protege.action.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.resource.*;
import edu.stanford.smi.protege.ui.*;
import edu.stanford.smi.protege.util.*;
import edu.stanford.smi.protege.storage.clips.*;

/**
 * Main class for the Protege application (as opposed to the applet).
 *
 * @author Ray Fergerson
 * @author Jennifer Vendetti
 */
public class Application {
    private final static char OPTION_CHAR 			= '-';
    private final static String INVERT_PREFIX 		= "no";
    private final static String TRACE_PREFIX 		= "trace";
    private final static String WARNINGS_PREFIX 	= "warn";
    private final static String STACK_PREFIX 		= "stack";
    private final static String BROWSER_PATH 		= "browser_path";

    private static JFrame _mainFrame;
    private static SplashScreen _splashScreen;
    private static WelcomeDialog _welcome;
    private static Properties _properties = new Properties();

    static {
        try {
            SystemUtilities.init();
            _properties.load(new FileInputStream(getPropertiesPath()));
        } catch (Exception e) {
            // Log.trace("unable to load properties file", Application.class, "static initializer");
        }
    }

    public static String getBrowserPath() {
        String path = _properties.getProperty(BROWSER_PATH);
        if (path == null) {
            path = promptForBrowserPath();
            if (path != null) {
                _properties.put(BROWSER_PATH, path);
                saveProperties();
            }
        }
        Log.trace(path, Application.class, "getBrowserPath");
        return path;
    }

    public static Component getMainWindow() {
        Component result;
        if (_splashScreen != null) {
            result = _splashScreen;
        } else {
            result = _mainFrame;
        }
        return result;
    }

    private static String getProjectFilePath(String[] args) {
        String filePath = null;
        for (int i = 0; i < args.length; ++i) {
            String arg = args[i];
            if (arg.charAt(0) != OPTION_CHAR) {
                filePath = arg;
                if (!filePath.endsWith(".pprj")) {
                    filePath += ".pprj";
                }
                break;
            }
        }
        if (filePath != null && filePath.startsWith("http:")) {
            int index = filePath.lastIndexOf('/');
            String path = filePath.substring(0, index);
            try {
                FileUtilities.setBaseURL(new java.net.URL(filePath));
            } catch (Exception e) {
                Log.exception(e, Application.class, "getProjectFilePath");
            }
            filePath = filePath.substring(index+1);
        }
        return filePath;
    }

    private static String getPropertiesPath() {
        return "protege.properties";
    }

    private static void init(String[] args) throws Exception {
        parseOptions(args);

        // Construct the application's main frame.
        _mainFrame = ComponentFactory.createMainFrame();
        _mainFrame.setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
        _mainFrame.addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent event) {
                ProjectManager.getProjectManager().exitApplicationRequest();
            }
        });

        // Determine the main frame's size on startup.
        ApplicationProperties.restoreMainFrameProperties(_mainFrame);
        Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
        Dimension frameSize = _mainFrame.getSize();
        frameSize.width = Math.min(frameSize.width, screenSize.width);
        frameSize.height = Math.min(frameSize.height, screenSize.height);
        _mainFrame.setSize(frameSize);

        // Find out if a filename was passed in on the command-line.
        ProjectManager.getProjectManager().setRootPane(_mainFrame.getRootPane());
        String projectFilePath = getProjectFilePath(args);

        if (projectFilePath != null) {
            // Load the project and show the main frame.
            ProjectManager.getProjectManager().loadProject(projectFilePath);
            showMainFrame();
        } else {
            // Check to see if the user wants to see the welcome dialog.
            boolean b = ApplicationProperties.getWelcomeDialogShow();
            if (b == true) {
                // Load the main frame and show the welcome dialog.
                _welcome = new WelcomeDialog(_mainFrame, Text.getProgramName(), true);
                showMainFrame();
                _welcome.setLocationRelativeTo(_mainFrame);
                _welcome.show();
            } else {
                // Just show Protege w/out the welcome dialog.
                showMainFrame();
            }
        }
    }

    public static void main(String[] args) {
        // Log.enter(Application.class, "main", args);
        _splashScreen = new SplashScreen();
        try {
            init(args);
        } catch (Exception e) {
            Log.exception(e, Application.class, "main");
        }
        _splashScreen.dispose();
        _splashScreen = null;
    }

    private static void parseOptions(String[] args) {
        for (int i = 0; i < args.length; ++i) {
            String arg = args[i];
            if (arg.length() > 1 && arg.charAt(0) == OPTION_CHAR) {
                boolean invert = false;
                arg = arg.substring(1);
                if (arg.startsWith(INVERT_PREFIX)) {
                    invert = true;
                    arg = arg.substring(INVERT_PREFIX.length());
                }
                if (arg.startsWith(TRACE_PREFIX)) {
                    boolean trace = true;
                    if (invert) {
                        trace = !trace;
                    }
                    Log.setDisplayTrace(trace);
                } else if (arg.startsWith(WARNINGS_PREFIX)) {
                    boolean warnings = true;
                    if (invert) {
                        warnings = !warnings;
                    }
                    Log.setDisplayWarnings(warnings);
                } else if (arg.startsWith(STACK_PREFIX)) {
                    boolean showStack = true;
                    if (invert) {
                        showStack = !showStack;
                    }
                    Log.setShowStack(showStack);
                } else {
                    Log.warning("invalid option: " + arg, Application.class, "parseOptions");
                }
            } else {
                break;
            }
        }
    }

    private static String promptForBrowserPath() {
        return "c:\\program files\\netscape\\communicator\\program\\netscape.exe";
    }

    public static void repaint() {
        if (_mainFrame != null) {
            _mainFrame.repaint();
        }
    }

    private static void saveProperties() {
        try {
            Log.enter(Application.class, "saveProperties");
            OutputStream os = new FileOutputStream(getPropertiesPath());
            _properties.store(os, null);
        } catch (Exception e) {
            Log.exception(e, Application.class, "saveProperties");
        }
    }

    private static void showMainFrame() {
        _splashScreen.setVisible(false);
        _mainFrame.show();
    }

    public static void shutdown() {
        _mainFrame.dispatchEvent(new WindowEvent(_mainFrame, WindowEvent.WINDOW_CLOSING));
    }

    private static void testSetFont() {
        String key = "Tree.font";
        Font font = UIManager.getFont(key);
        Font newFont = new Font(font.getName(), font.getStyle(), font.getSize() * 2);
        UIManager.put(key, newFont);
    }
}
