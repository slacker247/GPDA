
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
import java.util.*;
import browser.event.*;
import browser.model.*;
import edu.stanford.smi.protege.util.*;

public abstract class AbstractLibraryView extends JPanel implements LibraryListener {
  protected LibraryModel _library;
  protected Vector _listeners;

  public AbstractLibraryView() {
    _listeners = new Vector();
    _library = BrowserManager.getCurrentLibrary();
    initialize();
  }

  public void setLibraryModel(LibraryModel model) {
    _library = model;
    initialize();
  }

  public abstract String getName();

  public abstract void initialize();
  public abstract AbstractValidatableComponent getConfigurationPanel();
  public abstract String getConfigurationString();
  public abstract void loadConfigurationString();

  public abstract void selectionChanged(LibraryEvent event);
  public abstract void libraryModified(LibraryEvent event);
  public abstract void librarySaved(LibraryEvent event);
  public abstract void libraryClosed(LibraryEvent event);

  public void addListener(AbstractLibraryView view) {
    _listeners.add(view);
  }

  public void libraryChanged(LibraryEvent event) {
    _library = BrowserManager.getCurrentLibrary();
    repaint();
  }
}