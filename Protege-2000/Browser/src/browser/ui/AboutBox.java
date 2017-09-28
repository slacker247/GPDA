
/**
 * Title:        Browser<p>
 * Description:  This allows you to browse a library of knowledge bases.<p>
 * Copyright:    Copyright (c) Warren Shen<p>
 * Company:      Stanford Medical Informatics<p>
 * @author Warren Shen
 * @version 1.0
 */
package browser.ui;

import java.awt.*;
import javax.swing.JFrame;
import javax.swing.*;
import edu.stanford.smi.protege.util.*;
import edu.stanford.smi.protege.resource.*;

public class AboutBox extends JFrame {
  private JPanel _panel;

  public AboutBox() {
    super("About");
    init();
    int x = browser.ui.BrowserManager.getBrowserManager().getRootPane().getWidth() / 3;
    int y = browser.ui.BrowserManager.getBrowserManager().getRootPane().getHeight() / 3;
    this.setLocation(x, y);
    this.getContentPane().add(_panel);
    this.pack();
    this.setResizable(false);
    this.setIconImage(Icons.getNerd16x16Image());
    this.show();
  }

    public void init() {
      _panel = new JPanel();
      Font bigFont = new Font("Dialog", Font.BOLD, 28);
      Font font = new Font("Dialog", Font.PLAIN, 14);
      Font smallFont = new Font("Dialog", Font.PLAIN, 10);

      _panel.setLayout(new BorderLayout(0, 20));

      JPanel namePanel = new JPanel(new BorderLayout());
      namePanel.add(createLabel("Browser", bigFont), BorderLayout.EAST);
      JPanel tmPanel = new JPanel(new BorderLayout());
      tmPanel.add(createLabel("TM", font), BorderLayout.NORTH);

      JPanel n1 = new JPanel(new BorderLayout());
      n1.add(namePanel, BorderLayout.CENTER);
      n1.add(tmPanel, BorderLayout.EAST);

      JPanel name = new JPanel(new FlowLayout());
      name.add(n1);

      JPanel center = new JPanel(new BorderLayout());
      center.add(name, BorderLayout.NORTH);
      center.add(createCenteredLabel("Version 1.0", new Font("Dialog", Font.PLAIN, 20)), BorderLayout.CENTER);

      JPanel header = new JPanel(new BorderLayout());
      header.add(center, BorderLayout.CENTER);
      header.add(new JLabel(Icons.getNerd32x32Icon()), BorderLayout.WEST);
      _panel.add(header, BorderLayout.NORTH);
      JPanel helpPanel = new JPanel(new GridLayout(6, 1));
      helpPanel.add(createLabel("Send questions, bug reports, and suggestions to:", font));
      helpPanel.add(createCenteredText("protege-help@smi.stanford.edu"));
      helpPanel.add(new JLabel(""));
      helpPanel.add(createLabel("For more information see:", font));
      helpPanel.add(createCenteredText("http://protege.stanford.edu"));
      helpPanel.add(createCenteredText("and the tutorial included with this program"));
      _panel.add(helpPanel, BorderLayout.CENTER);

      JPanel legalPanel = new JPanel(new GridLayout(6, 1));
      legalPanel.add(createLabel("Developed by:", font));
      legalPanel.add(createCenteredText("Warren Shen"));
      legalPanel.add(createLabel("in affiliation with:", font));
      legalPanel.add(createCenteredText("Stanford Medical Informatics"));
      legalPanel.add(createLabel("", font));
      legalPanel.add(createLabel("Copyright © 2002, Stanford University", smallFont));

      _panel.add(legalPanel, BorderLayout.SOUTH);
    }

    private JLabel createCenteredLabel(String text, Font font) {
      JLabel label = createLabel(text, font);
      label.setHorizontalAlignment(JLabel.CENTER);
      return label;
    }

    private JComponent createCenteredText(String text) {
      JTextField field = new JTextField(text);
      field.setEnabled(false);
      field.setBackground(_panel.getBackground());
      field.setDisabledTextColor(_panel.getForeground());
      field.setBorder(null);
      field.setHorizontalAlignment(JTextField.CENTER);
      return field;
    }

    private JLabel createLabel(String text, Font font) {
      JLabel label = new JLabel(text);
      label.setFont(font);
      return label;
    }
}
