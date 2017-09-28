
/**
 * Title:        Browser<p>
 * Description:  This allows you to browse a library of knowledge bases.<p>
 * Copyright:    Copyright (c) Warren Shen<p>
 * Company:      Stanford Medical Informatics<p>
 * @author Warren Shen
 * @version 1.0
 */
package browser.action;

import javax.swing.*;
import browser.ui.*;
import java.awt.*;
import java.io.*;
import java.awt.event.*;
import edu.stanford.smi.protege.resource.*;
import edu.stanford.smi.protege.util.*;

public class SaveLibraryAsAction extends AbstractAction {
  private BrowserManager manager;

  public SaveLibraryAsAction(BrowserManager manager) {
    super("Save Library As...", Icons.getSaveProjectIcon());
    this.manager = manager;
  }

  public void actionPerformed(ActionEvent e) {
    manager.saveAs();
  }

}