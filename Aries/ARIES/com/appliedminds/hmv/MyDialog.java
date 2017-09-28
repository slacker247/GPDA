package com.appliedminds.hmv;

import java.awt.Frame;
import javax.swing.JDialog;
import javax.swing.event.EventListenerList;


/**
 * A custom JDialog that supports the registering of DialogHandler objects.
 *
 * @author daepark@apmindsf.com
 */
public class MyDialog extends JDialog {

  private EventListenerList _listenerList;


  /**
   * Creates a modal or non-modal dialog without a title and with the
   * specified owner Frame. If owner is null, a shared, hidden frame
   * will be set as the owner of the dialog.
   *
   * @param owner the Frame from which the dialog is displayed
   * @param modal true for a modal dialog, false for one that allows
   * others windows to be active at the same time
   */
  public MyDialog(Frame parent, boolean modal) {
    super(parent, modal);
    _listenerList = new EventListenerList();
  }


  /**
   * Add a DialogHandler object that wants to be nodified of
   * DialogHandler events.
   */
  public void addDialogHandler(DialogHandler handler) {
    _listenerList.add(DialogHandler.class, handler);
  }


  /**
   * Remove a DialogHandler object that wants to be nodified of
   * DialogHandler events.
   */
  public void removeDialogHandler(DialogHandler handler) {
    _listenerList.remove(DialogHandler.class, handler);
  }


  /**
   * Notify all registerd DialogHandler objects that the ok button was
   * pressed.
   */
  public void fireClickedOk() {
    // Guaranteed to return a non-null array
    Object[] listeners = _listenerList.getListenerList();
    // Process the listeners last to first, notifying
    // those that are interested in this event
    for (int i = listeners.length-2; i>=0; i-=2) {
      if (listeners[i] == DialogHandler.class) {
        // Lazily create the event:
        ((DialogHandler)listeners[i+1]).clickedOk();
      }
    }
  }


  /**
   * Notify all registerd DialogHandler objects that the apply button
   * was pressed.
   */
  public void fireClickedApply() {
    // Guaranteed to return a non-null array
    Object[] listeners = _listenerList.getListenerList();
    // Process the listeners last to first, notifying
    // those that are interested in this event
    for (int i = listeners.length-2; i>=0; i-=2) {
      if (listeners[i] == DialogHandler.class) {
        // Lazily create the event:
        ((DialogHandler)listeners[i+1]).clickedApply();
      }
    }
  }


  /**
   * Notify all registerd DialogHandler objects that the cancel button
   * was pressed.
   */
  public void fireClickedCancel() {
    // Guaranteed to return a non-null array
    Object[] listeners = _listenerList.getListenerList();
    // Process the listeners last to first, notifying
    // those that are interested in this event
    for (int i = listeners.length-2; i>=0; i-=2) {
      if (listeners[i] == DialogHandler.class) {
        // Lazily create the event:
        ((DialogHandler)listeners[i+1]).clickedCancel();
      }
    }
  }

} // end class "MyDialog"
