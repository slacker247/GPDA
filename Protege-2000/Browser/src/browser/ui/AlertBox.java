
/**
 * Title:        Browser<p>
 * Description:  This allows you to browse a library of knowledge bases.<p>
 * Copyright:    Copyright (c) Warren Shen<p>
 * Company:      Stanford Medical Informatics<p>
 * @author Warren Shen
 * @version 1.0
 */
package browser.ui;

import edu.stanford.smi.protege.util.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import java.util.*;

public class AlertBox extends JFrame {
  private static String log = "- Message History -\n";
  private String _text;
  private JTextArea _current;
  private JPanel _panel;
  private JScrollPane _scrollPane;

  public AlertBox(String message) {
    String time;

    Calendar calendar = new GregorianCalendar();
    time = calendar.getTime().toString();
    _text = time + " - " + message + "\n";
    log += _text;

    _panel = ComponentFactory.createPanel();

    _current = ComponentFactory.createTextArea();
    _current = new JTextArea(20, 30);
    _current.setBackground(Color.white);
    _current.setEditable(false);
    _current.setText(_text);
    _current.setAutoscrolls(true);
    _current.setWrapStyleWord(true);

    _scrollPane = ComponentFactory.createScrollPane(_current);
    _panel.add(_scrollPane, BorderLayout.CENTER);

    Box buttons = createButtonBox();

    getContentPane().add(_panel, BorderLayout.CENTER);
    getContentPane().add(buttons, BorderLayout.SOUTH);

    this.setTitle("Alerts");

    pack();
    show();
  }

  static public void addMessageToLog(String message) {
    log += message + "\n";
  }

  private Box createButtonBox() {
    JButton logButton = ComponentFactory.createButton(new AbstractAction("Show Log") {
      public void actionPerformed(ActionEvent e) {
        _current.setText(log);
      }
    });
    JButton currentButton = ComponentFactory.createButton(new AbstractAction("Show Current Message") {
      public void actionPerformed(ActionEvent e) {
        _current.setText(_text);
      }
    });
    JButton closeButton = ComponentFactory.createButton(new AbstractAction("Close") {
      public void actionPerformed(ActionEvent e) {
        // Yes, this is very ugly.
        // I wrote this when I didn't know how to do this properly
        // I am now too lazy to fix it
        ((AlertBox)(((Component)e.getSource()).getParent().getParent().getParent().getParent().getParent())).close();
      }
    });

    Box box = Box.createHorizontalBox();
    box.add(logButton);
    box.add(Box.createHorizontalGlue());
    box.add(currentButton);
    box.add(Box.createHorizontalGlue());
    box.add(closeButton);
    return box;
  }

  private void close() {
    this.dispose();
  }

}