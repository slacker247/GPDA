
/**
 * Title:        Browser<p>
 * Description:  This allows you to browse a library of knowledge bases.<p>
 * Copyright:    Copyright (c) Warren Shen<p>
 * Company:      Stanford Medical Informatics<p>
 * @author Warren Shen
 * @version 1.0
 */
package browser.action;

import java.awt.event.ActionEvent;
import javax.swing.*;
import edu.stanford.smi.protege.resource.*;
import edu.stanford.smi.protege.util.*;
import browser.ui.*;

public class ExitBrowserAction extends AbstractAction {
  private BrowserManager _manager;

  public ExitBrowserAction(BrowserManager manager) {
    super("Exit");
    _manager = manager;
  }

  public void actionPerformed(ActionEvent parm1) {
    _manager.requestExit();
  }
}