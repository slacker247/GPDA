package com.appliedminds.hmv;

import java.util.EventListener;


/**
 * Those who wish to add a handle on to the common dialog buttons
 * (such as "ok", "apply" and "cancel") can use this common interface.
 *
 * @author daepark@apmindsf.com
 */
public interface DialogHandler extends EventListener {

  void clickedOk();

  void clickedApply();

  void clickedCancel();

} // end interface "DialogHandler"

