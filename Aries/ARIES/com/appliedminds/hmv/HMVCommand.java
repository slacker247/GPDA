package com.appliedminds.hmv;


/**
 * Beginning of a command interface for the HMV app
 *
 * @author darin@apmindsf.com
 */
public interface HMVCommand {

  /**
   * Executes the implemented command
   */
  public void execute();


  /**
   * Reverses the operation carried out in the execute() method.
   */
  public void undo();


  /**
   * @return true if the operation encapsulated in this class can be reversed
   */
  public boolean isUndoable();


} // end interface "HMVCommand"
