
/**
 * Title:        Browser<p>
 * Description:  This allows you to browse a library of knowledge bases.<p>
 * Copyright:    Copyright (c) Warren Shen<p>
 * Company:      Stanford Medical Informatics<p>
 * @author Warren Shen
 * @version 1.0
 */
package browser.ui;

import javax.swing.Action;
import java.beans.PropertyChangeListener;
import java.awt.event.ActionEvent;

public class DummyEvent implements Action {

  public DummyEvent() {
  }

  public Object getValue(String key) {
    //TODO: Implement this javax.swing.Action method
    return "HI";
  }

  public void putValue(String key, Object value) {
    //TODO: Implement this javax.swing.Action method
  }

  public void setEnabled(boolean b) {
    //TODO: Implement this javax.swing.Action method
  }

  public boolean isEnabled() {
    //TODO: Implement this javax.swing.Action method
    return true;
  }

  public void addPropertyChangeListener(PropertyChangeListener listener) {
    //TODO: Implement this javax.swing.Action method
  }

  public void removePropertyChangeListener(PropertyChangeListener listener) {
    //TODO: Implement this javax.swing.Action method
  }

  public void actionPerformed(ActionEvent e) {
    //TODO: Implement this java.awt.event.ActionListener method
  }
}