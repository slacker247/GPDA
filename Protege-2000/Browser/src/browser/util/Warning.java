
/**
 * Title:        Browser<p>
 * Description:  This allows you to browse a library of knowledge bases.<p>
 * Copyright:    Copyright (c) Warren Shen<p>
 * Company:      Stanford Medical Informatics<p>
 * @author Warren Shen
 * @version 1.0
 */
package browser.util;

import javax.swing.*;
import java.awt.*;
import browser.*;
import java.util.*;

public class Warning {

  public Warning() {
  }

  private static void showWarning(String title, JPanel warning) {
    int x, y;
    JFrame frame = new JFrame("Prompt not found");
    frame.getRootPane().setLayout(new BorderLayout(10, 10));
    frame.getRootPane().add(warning);
    frame.setResizable(false);
    frame.pack();
    x = browser.ui.BrowserManager.getBrowserManager().getRootPane().getWidth() / 3;
    y = browser.ui.BrowserManager.getBrowserManager().getRootPane().getHeight() / 3;
    frame.setLocation(x, y);
    frame.show();
  }

  private static JLabel createCenteredLabel(String text) {
    JLabel label = new JLabel(text);
    label.setHorizontalAlignment(JLabel.CENTER);
    return label;
  }

  public static void showWarning(String title, String message) {
    Warning.showWarning(title, getWarningPanel(message));
  }

  private static JPanel getWarningPanel(String warning) {
    StringTokenizer st = new StringTokenizer(warning);
    String string = "";
    String token;
    ArrayList labels = new ArrayList();
    while(st.hasMoreTokens()) {
      token = st.nextToken();
      if(string.length() + token.length() < 40) {
        string += token;
        string += " ";
      } else {
        labels.add(Warning.createCenteredLabel(string));
        string = "";
      }
    }
    labels.add(Warning.createCenteredLabel(string));
    JPanel panel = new JPanel(new GridLayout(labels.size(), 0));
    for(int i = 0; i < labels.size(); i++) {
      panel.add((Component)labels.get(i));
    }
    return panel;
  }


  public static void warnNoPrompt() {
    JPanel panel = new JPanel(new GridLayout(5, 0));
    String dir = Browser.getProperties().getProperty(Browser.PROTEGE_DIR_PROPERTY);

    panel.add(Warning.createCenteredLabel("Warning: PROMPT plugin not found. Make"));
    panel.add(Warning.createCenteredLabel("sure that the Protege directory in the"));
    panel.add(Warning.createCenteredLabel("browser configuration (under the 'file'"));
    panel.add(Warning.createCenteredLabel("menu) is correct, and that the prompt plugin"));
    panel.add(Warning.createCenteredLabel("is in the Protege plugins directory."));

    Warning.showWarning("Prompt not found", panel);
  }

  public static void warnNoProtege() {
    JPanel panel = new JPanel(new GridLayout(4, 0));
    String dir = Browser.getProperties().getProperty(Browser.PROTEGE_DIR_PROPERTY);

    panel.add(Warning.createCenteredLabel("Warning: Protege application not found. Make"));
    panel.add(Warning.createCenteredLabel("sure that the Protege directory in the"));
    panel.add(Warning.createCenteredLabel("browser configuration (under the 'file'"));
    panel.add(Warning.createCenteredLabel("menu) is correct."));

    Warning.showWarning("Protege not found", panel);
  }

  public static void warnNoGraphviz() {
    JPanel panel = new JPanel(new GridLayout(4, 0));
    String dir = Browser.getProperties().getProperty(Browser.PROTEGE_DIR_PROPERTY);

    panel.add(Warning.createCenteredLabel("Warning: Graphviz executable not found."));
    panel.add(Warning.createCenteredLabel("Make sure that the Graphviz path in the"));
    panel.add(Warning.createCenteredLabel("browser configuration (under the 'file'"));
    panel.add(Warning.createCenteredLabel("menu) is correct."));

    Warning.showWarning("Graphviz not found", panel);
  }

}