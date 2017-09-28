
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
import com.borland.jbcl.layout.*;
import javax.swing.*;


/**
 * Possible searches:
 * Find all projects:
 */

public class QuerySelectorPanel extends JPanel {
  JLabel jLabel1 = new JLabel();
  JComboBox jComboBox1 = new JComboBox();
  JLabel jLabel2 = new JLabel();
  JTextField jTextField1 = new JTextField();
  GridBagLayout gridBagLayout1 = new GridBagLayout();

  public QuerySelectorPanel() {
    try {
      jbInit();
    } catch(Exception e) {
      new AlertBox(e.getMessage());
    }
  }

  private void jbInit() throws Exception {
    jLabel1.setText("Select projects where");
    this.setLayout(gridBagLayout1);
    jLabel2.setText("contains");
    jTextField1.setMaximumSize(new Dimension(4, 21));
    this.add(jTextField1, new GridBagConstraints(3, 0, 1, 1, 1.0, 0.0
            ,GridBagConstraints.WEST, GridBagConstraints.HORIZONTAL, new Insets(0, 0, 0, 0), 96, 15));
    this.add(jLabel1, new GridBagConstraints(0, 0, 1, 1, 0.0, 0.0
            ,GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), -18, 19));
    this.add(jComboBox1, new GridBagConstraints(1, 0, 1, 1, 1.0, 0.0
            ,GridBagConstraints.CENTER, GridBagConstraints.HORIZONTAL, new Insets(0, 0, 0, 0), -25, 15));
    this.add(jLabel2, new GridBagConstraints(2, 0, 1, 1, 0.0, 0.0
            ,GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 53, 19));
  }

}