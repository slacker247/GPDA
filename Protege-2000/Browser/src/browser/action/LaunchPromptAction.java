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
import java.awt.*;
import java.io.*;
import java.util.*;
import edu.stanford.smi.protege.util.*;
import edu.stanford.smi.protegex.prompt.*;
import edu.stanford.smi.protege.ui.*;
import edu.stanford.smi.protege.resource.Icons;
import edu.stanford.smi.protege.model.*;
import browser.model.*;
import browser.ui.*;

public class LaunchPromptAction extends AbstractAction {
  BrowserManager _manager;

  public static final int DIFF_MODE = 1000;
  public static final int MERGE_MODE = 1001;

  public LaunchPromptAction(BrowserManager manager) {
    super("Launch PROMPT", Icons.getCascadeWindowsIcon());
    _manager = manager;
  }

  public void actionPerformed(ActionEvent e) {
    _manager.launchPrompt();
  }

}
