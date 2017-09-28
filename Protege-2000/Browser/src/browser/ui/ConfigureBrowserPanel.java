
/**
 * Title:        Browser<p>
 * Description:  This allows you to browse a library of knowledge bases.<p>
 * Copyright:    Copyright (c) Warren Shen<p>
 * Company:      Stanford Medical Informatics<p>
 * @author Warren Shen
 * @version 1.0
 */
package browser.ui;

import browser.Browser;
import browser.model.*;
import edu.stanford.smi.protege.util.*;
import java.util.*;
import javax.swing.*;
import java.awt.event.*;
import java.awt.*;
import java.io.*;

public class ConfigureBrowserPanel extends ValidatableTabComponent {
  BrowserManager _bm;
  OptionsPanel _panel;

  public ConfigureBrowserPanel(BrowserManager bm) {
    _bm = bm;
    AbstractLibraryView view;
    AbstractValidatableComponent configPanel;
    Iterator iter = bm.getLibraryViews().iterator();
    Hashtable ht = new Hashtable();

    addTab("General", new ConfigureBrowserTab());
    addTab("Library Item Type", getLibraryItemTab());
    while(iter.hasNext()) {
      view = (AbstractLibraryView) iter.next();
      if(!ht.containsKey(view.getClass().getName())) {
        ht.put(view.getClass().getName(), view.getClass().getName());
        configPanel = view.getConfigurationPanel();
        if(configPanel != null) {
          addTab(view.getName(), configPanel);
        }
      }
    }
  }

  private AbstractValidatableComponent getLibraryItemTab() {
    ArrayList list = new ArrayList();
    Iterator iter = _bm.getCurrentLibrary().getItemTypes().iterator();
    LibraryItem item;

    while(iter.hasNext()) {
      item = (LibraryItem)iter.next();
      list.add(item.getTypeName());
    }
    _panel = new OptionsPanel(list,
      CollectionUtilities.createCollection(_bm.getCurrentLibrary().getCurrentItemType()),
      "Type", "Select a Library Item Type", false);
    return _panel;
  }

  public void saveContents() {
    super.saveContents();
  }

  private class ConfigureBrowserTab extends AbstractValidatableComponent {
    private Box _box;
    private Hashtable _properties;
    private JTextField _field;
    private String _string;

    public ConfigureBrowserTab() {
      super();
      init();
    }

    private void init() {
      String value;
      Properties p = Browser.getProperties();
      _box = Box.createVerticalBox();
      _properties = new Hashtable();

      this.setLayout(new BorderLayout());

      _box.add(getField("Protege Directory", Browser.PROTEGE_DIR_PROPERTY,
          p.getProperty(Browser.PROTEGE_DIR_PROPERTY),
          "The directory where your Protege is located"));

      _box.add(getField("Protege Command", Browser.PROTEGE_CMD_PROPERTY,
          p.getProperty(Browser.PROTEGE_CMD_PROPERTY),
          "The command to run Protege"));

      JPanel note = new JPanel(new GridLayout(5, 1));
      note.add(new JLabel("The paths below may be either absolute or relative.  If the paths are not absolute"));
      note.add(new JLabel("paths, they are relative to the working directory:"));
      note.add(new JLabel(""));
      note.add(new JLabel(ApplicationProperties.getApplicationDirectory()));
      note.add(new JLabel(""));
      _box.add(note);


      _box.add(getField("Graphviz Executable", Browser.DOT_COMMAND_PROPERTY,
          p.getProperty(Browser.DOT_COMMAND_PROPERTY),
          "The path of the 'dot.exe' executable that runs Graphviz"));

      _box.add(getField("Plugins Directory", Browser.LIBRARY_ITEMS_PATH_PROPERTY,
          p.getProperty(Browser.LIBRARY_ITEMS_PATH_PROPERTY),
          "The directory where the plugins that define other supported library types"));

      add(_box, BorderLayout.CENTER);
    }

    private LabeledComponent getField(String label, String name, String value, String tooltip) {
      _string = name;
      _field = new JTextField(value);
      _field.setText(value);
      _properties.put(name, _field);

      LabeledComponent lc = new LabeledComponent(label, _field);
      lc.setToolTipText(tooltip);
      return lc;
    }

    public boolean validateContents() {
      return true;
    }

    public void saveContents() {
      try {
        String file = ApplicationProperties.getApplicationDirectory() + "\\" + Browser.PROPERTIES_FILE;
        PrintWriter pw = new PrintWriter(FileUtilities.createWriter(new File(file)));
        String name, value;
        pw.println("# Browser Properties");
        pw.println("# Last modified: " + (new Date(System.currentTimeMillis())));
        pw.println("");
        Iterator iter = _properties.keySet().iterator();
        Enumeration enum = _properties.keys();
        while(enum.hasMoreElements()) {
          name = (String)enum.nextElement();
          value = ((JTextField)_properties.get(name)).getText();
          value = fixSlashes(value);
          pw.println(name + " = " + value);
          Browser.getProperties().setProperty(name, value);
        }
        updateBrowserManager();
        pw.flush();
      } catch (IOException e) {
        System.err.println("Could not write properties to file.");
      }
    }

    private String fixSlashes(String value) {
      String newVal = "";
      char c, c2;
      boolean lastWasSlash = false;
      int i = 0;
      while(i < value.length()) {
        c = value.charAt(i);
        if(c == '\\') {
          if(!lastWasSlash) {
            c2 = ' ';
            if(i < value.length()-1) c2 = value.charAt(i+1);
            if(c2 != '\\') newVal += '\\';
          }
          lastWasSlash = true;
        } else {
          lastWasSlash = false;
        }
        newVal += c;
        i++;
      }
      return newVal;
    }

    private void updateBrowserManager() {
      _bm.addItemTypes(); // in case we changed the plugins directory at runtime
      _bm.getCurrentLibrary().setItemType( // in case we changed the library item type
        (String)CollectionUtilities.getSoleItem(_panel.getSelectedOptions()));

    }
  }
}