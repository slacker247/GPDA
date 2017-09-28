
/**
 * Title:        Browser<p>
 * Description:  This allows you to browse a library of knowledge bases.<p>
 * Copyright:    Copyright (c) Warren Shen<p>
 * Company:      Stanford Medical Informatics<p>
 * @author Warren Shen
 * @version 1.0
 */
package browser.ui;

import javax.swing.*;
import edu.stanford.smi.protege.action.*;
import javax.swing.JMenuBar;
import edu.stanford.smi.protege.util.*;
import browser.ui.*;
import browser.action.*;

public class BrowserMenuBar extends JMenuBar {
  BrowserManager _browserManager;

  public BrowserMenuBar(BrowserManager manager) {
    this._browserManager = manager;
    createFileMenu();
    createMiscMenu();
  }

  private void createFileMenu() {
    JMenu menu = ComponentFactory.createMenu();
    menu.setText("File");
    ComponentFactory.addMenuItem(menu, new AddFileAction(_browserManager));
    ComponentFactory.addMenuItem(menu, new RemoveItemAction(_browserManager));
    menu.addSeparator();
    ComponentFactory.addMenuItem(menu, new NewLibraryAction(_browserManager));
    ComponentFactory.addMenuItem(menu, new LoadLibraryAction(_browserManager));
    ComponentFactory.addMenuItem(menu, new SaveLibraryAction(_browserManager));
    ComponentFactory.addMenuItem(menu, new SaveLibraryAsAction(_browserManager));
    menu.addSeparator();
    ComponentFactory.addMenuItem(menu, new ConfigureBrowserAction(_browserManager));
    menu.addSeparator();
    ComponentFactory.addMenuItem(menu, new ExitBrowserAction(_browserManager));

    add(menu);
  }

  private void createMiscMenu() {
    JMenu menu = ComponentFactory.createMenu();
    menu.setText("About");
    char sep = java.io.File.separatorChar;

    ComponentFactory.addMenuItem(menu, new ShowAboutBoxAction(_browserManager));
    add(menu);
  }

}