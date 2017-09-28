
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
import javax.swing.*;

public class QueryResultPanel extends JPanel {
  private BorderLayout borderLayout1 = new BorderLayout();
  private JTextArea jTextArea1 = new JTextArea();

  public QueryResultPanel() {
    try {
      jbInit();
    }
    catch(Exception ex) {
      ex.printStackTrace();
    }
  }

  void jbInit() throws Exception {
    jTextArea1.setText("Query Results will go here");
    this.setLayout(borderLayout1);
    this.add(jTextArea1, BorderLayout.CENTER);
  }
}