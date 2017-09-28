
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
import edu.stanford.smi.protege.resource.*;
import java.awt.*;
import javax.swing.*;
import java.awt.event.*;
import browser.util.*;

public class StatusBox extends SwingWorker {
  private JFrame _frame;
  private JProgressBar _progressBar = null;
  private JPanel _panel;
  private Box _textBox;
  private JLabel _text;
  private Timer _timer;
  private int _taskLevel;
  private String _title;
  private boolean _showBar;

  public final static int ONE_SECOND = 1000;


  public StatusBox(String title, String text, boolean showBar) {
    _showBar = showBar;
    _title = title;
    init(title, showBar);
    setText(text);
  }

  public Object construct() {
    init(_title, _showBar);
    return this;
  }

  private void init(String title, boolean showBar) {
    _frame = new JFrame(title);
    _frame.getRootPane().setLayout(new BorderLayout());
    if(showBar) {
      _progressBar = new JProgressBar(0, 100);
    }
    initPanel();
    _frame.getRootPane().add(_panel, BorderLayout.CENTER);

    _taskLevel = 0;
    _timer = new Timer(ONE_SECOND, new ActionListener() {
      public void actionPerformed(ActionEvent evt) {
        _progressBar.setValue(_taskLevel);
        if (_taskLevel >= 100) {
          Toolkit.getDefaultToolkit().beep();
          _timer.stop();
        }
      }
    });
    _timer.start();

    _frame.setResizable(false);
    _frame.pack();
    _frame.show();
  }

  private void initPanel() {
    _panel = ComponentFactory.createPanel();
    _panel.setLayout(new BorderLayout());
    initTextBox();
    _panel.add(_textBox, BorderLayout.CENTER);
    if(_progressBar != null) {
      _panel.add(_progressBar, BorderLayout.SOUTH);
    }
    _panel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
    _panel.setPreferredSize(new Dimension(200, 60));
  }

  private void initTextBox() {
    _text = new JLabel("");
    _textBox = Box.createHorizontalBox();
    _textBox.add(Box.createHorizontalGlue());
    _textBox.add(_text);
    _textBox.add(Box.createHorizontalGlue());
  }

  public void setText(String text) {
    _text.setText(text);
  }

  public void setBar(double percent) {
    _taskLevel = (int)(percent * 100);
  }

  public void close() {
    _frame.dispose();
  }

}