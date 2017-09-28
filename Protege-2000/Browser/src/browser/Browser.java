/**
 * Title:        Browser<p>
 * Description:  This allows you to browse a library of knowledge bases.<p>
 * Copyright:    Copyright (c) Warren Shen<p>
 * Company:      Stanford Medical Informatics<p>
 * @author Warren Shen
 * @version 1.0
 */
package browser;

import browser.util.*;
import browser.model.*;
import browser.ui.*;
import java.util.*;
import java.awt.*;
import javax.swing.*;
import java.awt.event.*;
import java.io.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.util.*;
import com.sun.java.swing.plaf.windows.*;


public class Browser {

  public static final String PROPERTIES_FILE = "browser.properties";
  public static final String DOT_COMMAND_PROPERTY = "dot.command";
  public static final String PROTEGE_CMD_PROPERTY = "protege.cmd";
  public static final String PROTEGE_DIR_PROPERTY = "protege.dir";
  public static final String LIBRARY_ITEMS_PATH_PROPERTY = "items.path";
  public static final String LIBRARY_ITEM_ENTRY_PROPERTY = "LibraryItem";

  private static Properties _properties;

  private BrowserManager _bm;

  static {
    try {
      Browser._properties = new Properties();
      BufferedInputStream s = new BufferedInputStream(new FileInputStream(Browser.PROPERTIES_FILE));
      Browser._properties.load(s);
    } catch (Exception e2) {
      System.err.println("Warning: Could not find 'browser.properties' file.  Make ");
      System.err.println("sure it is in the same directory as 'browser.jar' file.");
    }
  }

  public Browser() {
    graphicalUI();
  }

  public static Properties getProperties() {
    return Browser._properties;
  }


  public static void main(String[] args) {
    Browser b = new Browser();
  }

  private void textUI() {
    LibraryModel library = new LibraryModel();
  }

  private void graphicalUI() {
    _bm = new BrowserManager();
    JFrame frame = ComponentFactory.createMainFrame();
    _bm.setRootPane(frame.getRootPane());
    _bm.displayLibrary();
    frame.addWindowListener(new WindowAdapter() {
      public void windowClosing(WindowEvent event) {
        _bm.requestExit();
      }
    });
    frame.setSize(600, 450);
    frame.setTitle("Library Browser");
    frame.pack();
    frame.show();

  }
}