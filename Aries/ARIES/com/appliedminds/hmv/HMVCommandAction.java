package com.appliedminds.hmv;

import java.awt.event.ActionEvent;
import javax.swing.AbstractAction;

/**
 * An Action implementation that works with an HMVCommand. The HMVCommand's
 * execute() method is called when the Action is invoked
 *
 * @author darin@apmindsf.com
 */
public class HMVCommandAction extends AbstractAction {

  /** the command that will be executed */
  private HMVCommand _command;


  /**
   * Creates a new HMVCommandAction
   *
   * @param command the HMVCommand to be executed when the Action is invoked
   */
  public HMVCommandAction(HMVCommand command) {
    _command = command;
  }


  public void actionPerformed(ActionEvent e) {
    _command.execute();
  }


} // end class
