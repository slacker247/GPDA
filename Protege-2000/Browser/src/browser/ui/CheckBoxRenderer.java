
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
import javax.swing.table.*;

class CheckBoxRenderer extends JCheckBox implements TableCellRenderer {

    public CheckBoxRenderer() {
        setHorizontalAlignment(CENTER);
        setOpaque(false);
    }

    public Component getTableCellRendererComponent(JTable table, Object value,
            boolean isSelected, boolean b, int row, int column) {
        setSelected(Boolean.TRUE.equals(value));
        return this;
    }
}
