package com.appliedminds.hmv;


/**
 * A convenience implementation of a DialogHandler. All implemented
 * methods do nothing. Subclasses must overwrite to do anything
 * useful.
 *
 * @author daepark@apmindsf.com
 */
public class DialogHandlerAdapter implements DialogHandler {

  public void clickedOk() { }

  public void clickedApply() { }

  public void clickedCancel() { }

} // end interface "DialogHandlerAdapter"

