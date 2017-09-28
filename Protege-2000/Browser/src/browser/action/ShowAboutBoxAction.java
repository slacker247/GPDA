
/**
 * Title:        Browser<p>
 * Description:  This allows you to browse a library of knowledge bases.<p>
 * Copyright:    Copyright (c) Warren Shen<p>
 * Company:      Stanford Medical Informatics<p>
 * @author Warren Shen
 * @version 1.0
 */
package browser.action;

import browser.ui.BrowserManager;
import browser.ui.AboutBox;
import browser.ui.*;
import javax.swing.*;
import java.awt.event.ActionEvent;

public class ShowAboutBoxAction extends AbstractAction {

  public ShowAboutBoxAction(BrowserManager browserManger) {
    super("About");
  }

  public void actionPerformed(ActionEvent parm1) {
    new AboutBox();
  }
}