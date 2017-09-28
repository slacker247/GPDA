
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

public class SetLibraryItemTypeAction extends AbstractAction{
  private BrowserManager _manager;

  public SetLibraryItemTypeAction(BrowserManager manager) {
    super("Set Item Type", Icons.getRemoveSlotOverrideIcon());
    this._manager = manager;
  }

  public void actionPerformed(ActionEvent e) {
    _manager.setLibraryItemType();
  }
}