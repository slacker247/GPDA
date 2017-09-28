/*
 * Title:        Browser<p>
 * Description:  This allows you to browse a library of knowledge bases.<p>
 * Copyright:    Copyright (c) Warren Shen<p>
 * Company:      Stanford Medical Informatics<p>
 * @author Warren Shen
 * @version 1.0
 */
package browser.ui;

import javax.swing.event.*;
import java.awt.*;
import browser.model.*;
import javax.swing.*;
import browser.ui.*;
import browser.event.*;
import java.util.*;
import edu.stanford.smi.protege.ui.*;
import edu.stanford.smi.protege.util.*;
import edu.stanford.smi.protege.resource.*;
import edu.stanford.smi.protege.model.*;
import browser.action.*;
import browser.query.*;
import browser.ui.menu.*;
import browser.util.*;
import java.awt.event.*;
import java.io.*;
import java.net.*;
import browser.Browser;

public class BrowserManager {
  private JRootPane _rootPane;
  private static LibraryModel _library;
  private static BrowserManager _manager;
  private Collection _libraryViews;

  public BrowserManager() {
    _library = new LibraryModel();
    _libraryViews = new ArrayList();
    addItemTypes();
    _manager = this;
  }

  public void setRootPane(JRootPane rootPane) {
    _rootPane = rootPane;
    _rootPane.getContentPane().setLayout(new BorderLayout());
    _rootPane.setJMenuBar(new BrowserMenuBar(this));
  }

  public static BrowserManager getBrowserManager() {
    return _manager;
  }

  public void setCurrentLibrary(LibraryModel model) {
    _library.setFileName(model.getFileName());
    while(_library.size() > 0) {
      _library.removeItem(0);
    }
    _library.addLibraryItems(model.getAllItems());
    _library.setSelectedKBSummaries(new ArrayList());
    String title;
    if(model.getFileName().equals("")) {
      title = "Library Browser";
    } else {
      title = "LibraryBrowser - " + model.getFileName();
    }
    ((JFrame)_rootPane.getParent()).setTitle(title);
    _library.postLibraryEvent(LibraryEvent.LIBRARY_CHANGED);
  }

  public static LibraryModel getCurrentLibrary() {
    return _library;
  }

  public void configureBrowser() {
    ConfigureBrowserPanel panel = new ConfigureBrowserPanel(this);
    String title = "Configure Browser";

    int result = ModalDialog.showDialog(_rootPane, panel, title, ModalDialog.MODE_OK_CANCEL);
  }

  public void displayLibrary() {
    BrowserView view = new BrowserView(_library);
    _rootPane.getContentPane().add(view, BorderLayout.CENTER);
    _libraryViews = view.getLibraryViews();
    view.revalidate();
    view.repaint();

  }

  public void getUserLibraryItems() {
    _library.getUserLibraryItems();
  }

  public void requestExit() {
    java.awt.Frame mainFrame = ComponentUtilities.getFrame(_rootPane);
    ApplicationProperties.recordMainFrameProperties(mainFrame);
    ApplicationProperties.flush();
    ComponentUtilities.dispose(mainFrame);
    SystemUtilities.gc();
    SystemUtilities.exit();
  }

  public void addItemTypes() {
    _library.addItemType(new KBSummary());

    try {
      String itemName;
      JarClassLoader jcloader;
      File[] jars = JarClassLoader.getJarsInDirectory(
        (String)Browser.getProperties().get(Browser.LIBRARY_ITEMS_PATH_PROPERTY));
      for(int i = 0; i < jars.length; i++) {
        if(!jars[i].getName().equals("prompt.jar")) {  // my one hack in this program
          jcloader = new JarClassLoader(new URL("file:\\" + jars[i].getAbsolutePath()));
          itemName = JarClassLoader.getItemTypeName(jars[i]);
          _library.addItemType((LibraryItem)jcloader.getNewLibraryItem(itemName));
        }
      }
    } catch (Exception e) {

    }
  }

  public void setLibraryItemType() {
    ArrayList list = new ArrayList();
    Iterator iter = _library.getItemTypes().iterator();
    LibraryItem item;
    while(iter.hasNext()) {
      item = (LibraryItem)iter.next();
      list.add(item.getTypeName());
    }
    OptionsPanel panel = new OptionsPanel(list,
      CollectionUtilities.createCollection(_library.getCurrentItemType()),
      "Type", "Select a Library Item type", false);
    int result = ModalDialog.showDialog(_rootPane, panel,
                                        "Set Type", ModalDialog.MODE_OK_CANCEL);
    if (result == ModalDialog.OPTION_OK) {
      _library.setItemType((String)CollectionUtilities.getSoleItem(panel.getSelectedOptions()));
    }
  }

  public static Collection launchFileChooser(String type, String extension) {
    return BrowserManager.launchFileChooser(type, extension, type, "Open", true);
  }

  public static Collection launchFileChooser(String type, String extension, String title,
                                              String buttonText, boolean allowDirectories) {
    ArrayList fileNames = new ArrayList();
    FileUtilities.setCurrentWorkingDirectoryFromFile(
      Browser.getProperties().getProperty(Browser.PROTEGE_DIR_PROPERTY) + "\\projects\\");
    JFileChooser chooser = ComponentFactory.createFileChooser(type, extension);
    if(allowDirectories) {
      chooser.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES);
    } else {
      chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
    }
    chooser.setDialogTitle(title);
    chooser.setFileFilter(new ExtensionFilter(extension, type));
    chooser.setMultiSelectionEnabled(true);
    chooser.setApproveButtonText(buttonText);
    int rval = chooser.showOpenDialog(new JPanel());
    if (rval == JFileChooser.APPROVE_OPTION && chooser.getSelectedFile() != null) {
      if(chooser.getSelectedFiles().length == 0) {
        return CollectionUtilities.createCollection(chooser.getSelectedFile().getAbsolutePath());
      }
      if(chooser.getSelectedFiles()[0].isDirectory()) {
        File [] temp = chooser.getSelectedFile().listFiles();
        for(int i = 0; i < temp.length; i++) {
          if(FileUtilities.getExtension(temp[i].getName()).endsWith(extension)) {
            fileNames.add(temp[i].getAbsolutePath());
          }
        }
      } else {
        File [] files = chooser.getSelectedFiles();
        for(int i = 0; i < files.length; i++) {
          fileNames.add(files[i].getAbsolutePath());
        }
        return fileNames;
      }
    }
    return fileNames;
  }

  public static void launchProtege(String fileName) {
    try {
      String protegeCmd = Browser.getProperties().getProperty(Browser.PROTEGE_CMD_PROPERTY);
      if(protegeCmd == null) {
        Warning.warnNoProtege();
        return;
      }
      if(fileName == null) {
        return;
      }
      String project = "\"";
      project += fileName;
      project += "\"";
      String[] command =
        new String[] {protegeCmd, project};
      Process process = Runtime.getRuntime().exec(command);
      process.getErrorStream().close();
      process.getInputStream().close();
    } catch (IOException exception) {
        Warning.warnNoProtege();
    }
  }


  public void launchPrompt() {
    if(!hasPrompt()) {
      Warning.warnNoPrompt();
      return;
    }

    ArrayList list = new ArrayList();
    list.add("DIFF Mode");
    list.add("MERGE mode");
    OptionsPanel panel = new OptionsPanel(list,
                                          CollectionUtilities.createCollection("DIFF Mode"),
                                          "Mode", "Pick a mode for PROMPT", false);
    String title = "Pick Prompt Mode";
    int result = ModalDialog.showDialog(_rootPane, panel, title, ModalDialog.MODE_OK_CANCEL);
    if (result == ModalDialog.OPTION_OK) {
      String selected = (String)CollectionUtilities.getSoleItem(panel.getSelectedOptions());
      if(selected.equals("DIFF Mode")) {
        doPrompt(LaunchPromptAction.DIFF_MODE);
      } else {
        doPrompt(LaunchPromptAction.MERGE_MODE);
      }
    }
  }

  private boolean hasPrompt() {
    String dir = Browser.getProperties().getProperty(Browser.PROTEGE_DIR_PROPERTY);
    if(dir == null) return false;
    File file = new File(dir + "\\plugins");
    File []jars = file.listFiles();
    for(int i = 0; i < jars.length; i++) {
      file = jars[i];
      if(file.getName().endsWith("prompt.jar")) return true;
    }
    return false;
  }

  private void doPrompt(int mode) {
    try {
      if(_library.getSelectedKBSummaries() == null) {
        return;
      }
      String protegeDir = browser.Browser.getProperties().getProperty(Browser.PROTEGE_DIR_PROPERTY);
      if(protegeDir == null) {
        Warning.warnNoPrompt();
        protegeDir = "C:\\Program Files\\Protege-2000";
      } else {
        System.err.println("Invoking Protege at: " + protegeDir);
      }
      ArrayList selected = new ArrayList(_library.getSelectedKBSummaries());
      String filePath1 = ((LibraryItem)selected.get(0)).getFilePath();
      String filePath2 = ((LibraryItem)selected.get(1)).getFilePath();
      String mergedProject = createTempMergedProject();

      String cmdString = "java -cp ";
      cmdString = cmdString.concat("\"" + protegeDir + "\\protege.jar\" ");
      cmdString = cmdString.concat("-Dprotege.dir=\"" + protegeDir + "\" ");
      cmdString = cmdString.concat("-Dprompt.project1=\"" + filePath1 + "\" ");
      cmdString = cmdString.concat("-Dprompt.project2=\"" + filePath2 + "\" ");
      if(mode == LaunchPromptAction.DIFF_MODE) {
        cmdString = cmdString.concat("-Dprompt.mode=DIFF_MODE ");
      }
      if(mode == LaunchPromptAction.MERGE_MODE) {
        cmdString = cmdString.concat("-Dprompt.mode=MERGE_MODE ");
      }
      cmdString = cmdString.concat("edu.stanford.smi.protege.Application ");
      cmdString = cmdString.concat(mergedProject);

      Process process = Runtime.getRuntime().exec(cmdString);

    } catch (Exception exception) {
      exception.printStackTrace();
    }
  }

  private String createTempMergedProject() {
    Project p;
    KBSummary selected;
    String fileName = "";
    try {
      File f = File.createTempFile("merged", ".pprj");
      fileName = f.getPath();
      selected = (KBSummary)CollectionUtilities.getFirstItem(_library.getSelectedKBSummaries());
      p = Project.createNewProject(selected.getKnowledgeBaseFactory(), new ArrayList());
      p.setProjectFilePath(fileName);
      Iterator iter = p.getTabWidgetDescriptors().iterator();

      WidgetDescriptor d;
      ArrayList tabs = new ArrayList();

      while(iter.hasNext()) {
        d = (WidgetDescriptor)iter.next();
        if(d.getWidgetClassName().equals("edu.stanford.smi.protegex.prompt.PromptTab")) {
          d.setVisible(true);
          tabs.add(d);
        }
      }

      p.setTabWidgetDescriptorOrder(tabs);
      p.save(new ArrayList());

      p.dispose();
    } catch (Exception e) {
      e.printStackTrace();
    }
    return fileName;
  }

  public Collection getLibraryViews() {
    return _libraryViews;
  }

  public void removeSelectedItems() {
    _library.removeSelectedItems();
  }


  public void saveLibrary() {
    String file = _library.getFileName();
    if(!file.equals("")) {
      saveLibraryToFile(file);
    } else {
      saveAs();
    }

  }

  public void saveAs() {
    Collection selection =
    BrowserManager.launchFileChooser("Protege Library", ".plib", "Save Library As...", "Save", false);
    if(selection.isEmpty()) {
      return;
    }
    String file = (String)CollectionUtilities.getFirstItem(selection);
    if(!file.endsWith(".plib")) file = file + ".plib";
    saveLibraryToFile(file);
  }

  public void saveLibraryToFile(String file) {
    try {
      PrintWriter pw = new PrintWriter(FileUtilities.createWriter(new File(file)));
      writeItemsToFile(pw);
      writeConfigurationToFile(pw);
      _library.setFileName(file);
      ((JFrame)_rootPane.getParent()).setTitle("Library Browser - " + _library.getFileName());
      pw.flush();
      pw.close();
    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  private void writeItemsToFile(PrintWriter pw) {
    Iterator iter = _library.getAllItems().iterator();
    LibraryItem item;
    while(iter.hasNext()) {
      item = (LibraryItem) iter.next();
      pw.println("{");
      pw.println(item.getTypeName());
      pw.println(item.getIdentifier());
      pw.println(item.isIncluded());
      pw.println(item.isActive());
      pw.println("}");
    }
  }

  private void writeConfigurationToFile(PrintWriter pw) {
    Iterator iter = _libraryViews.iterator();
    AbstractLibraryView view;
    while(iter.hasNext()) {
      view = (AbstractLibraryView)iter.next();
      pw.println("[");
      pw.println(view.getName());
      pw.println(view.getConfigurationString());
      pw.println("]");
    }
  }

  public void loadLibraryFromFile() {
    Collection selection =
      BrowserManager.launchFileChooser("Library", ".plib", "Load Library", "Open", false);
    if(selection.isEmpty()) {
      return;
    }
    try {
      String filename = ((String)CollectionUtilities.getFirstItem(selection));
      BufferedReader reader = new BufferedReader(new FileReader(filename));
      String line;
      LibraryModel model = new LibraryModel(_libraryViews);
      while(true) {
        line = reader.readLine();
        if(line == null) break;
        if(line.startsWith("{")) {
          model.addLibraryItem(loadItem(reader));
        }
        if(line.startsWith("[")) {
          loadConfiguration(reader);
        }
      }
      model.setFileName(filename);
      this.setCurrentLibrary(model);
    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  private LibraryItem loadItem(BufferedReader reader) {
    LibraryItem item;
    try {
      LibraryItem type = _library.getItemType(reader.readLine());
      if(type == null) {
        while(!reader.readLine().startsWith("}"));
        return null;
      }
      boolean hidden, active;
      String shidden, sactive;
      String name = reader.readLine();
      shidden = reader.readLine();
      sactive = reader.readLine();
      hidden = Boolean.valueOf(shidden).booleanValue();
      active = Boolean.valueOf(sactive).booleanValue();
      item = (LibraryItem)CollectionUtilities.getFirstItem(
        type.createNewLibraryItems(CollectionUtilities.createCollection(name),
          hidden, active));
    } catch (IOException e) {
      e.printStackTrace();
      item = null;
    }
    return item;
  }


  private void loadConfiguration(BufferedReader reader) {
    try {
      while(reader.ready()) {
        if(reader.readLine().startsWith("]")) {
          return;
        }
      }
    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  public JRootPane getRootPane() {
    return this._rootPane;
  }

}