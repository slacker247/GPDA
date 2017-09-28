
/**
 * Title:        Browser<p>
 * Description:  This allows you to browse a library of knowledge bases.<p>
 * Copyright:    Copyright (c) Warren Shen<p>
 * Company:      Stanford Medical Informatics<p>
 * @author Warren Shen
 * @version 1.0
 */
package browser.action;

import java.awt.event.*;
import javax.swing.*;
import java.io.*;
import edu.stanford.smi.protege.resource.Icons;
import edu.stanford.smi.protegex.widget.pal.*;
import browser.ui.LibraryTable;
import browser.model.*;
import edu.stanford.smi.protege.util.*;

public class ShowInactiveItemsAction extends AbstractAction {
  private LibraryTable _table;

  public ShowInactiveItemsAction(LibraryTable table) {
    super("Show inactive items", PalIcons.getCheckAllIcon());
    _table = table;
  }

  public void actionPerformed(ActionEvent e) {
    _table.setMode(LibraryTable.SHOW_INACTIVE);
  }
}