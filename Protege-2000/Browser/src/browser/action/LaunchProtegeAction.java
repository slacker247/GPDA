
/**
 * Title:        Browser<p>
 * Description:  This allows you to browse a library of knowledge bases.<p>
 * Copyright:    Copyright (c) Warren Shen<p>
 * Company:      Stanford Medical Informatics<p>
 * @author Warren Shen
 * @version 1.0
 */
package browser.action;

import browser.Browser;
import java.awt.event.*;
import javax.swing.*;
import java.io.*;
import edu.stanford.smi.protege.resource.Icons;
import browser.model.LibraryModel;
import browser.model.*;
import edu.stanford.smi.protege.util.*;
import browser.ui.*;

public class LaunchProtegeAction extends AbstractAction {
  private LibraryModel _library;

  public LaunchProtegeAction(LibraryModel library) {
    super("Launch Protege", Icons.getNerd16x16Icon());
    _library = library;
  }

  public void actionPerformed(ActionEvent e) {
    if(_library.getSelectedKBSummaries() == null || _library.getSelectedKBSummaries().size() == 0) {
      return;
    }
    LibraryItem item = (LibraryItem)CollectionUtilities.getFirstItem(_library.getSelectedKBSummaries());
    BrowserManager.launchProtege(item.getFilePath());
  }
}