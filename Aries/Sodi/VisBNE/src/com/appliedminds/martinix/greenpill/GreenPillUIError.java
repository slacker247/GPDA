package com.appliedminds.martinix.greenpill;


import com.appliedminds.martini.MartiniError;


/*
 * For runtime errors in the GreenPill system that we don't expect to
 * be caught.
 *
 *
 * @author mathias@apmdinsf.com
 * @author daepark@apmindsf.com
 */
public class GreenPillUIError extends MartiniError {


  /** 
   * Create an error with some detail message.
   *
   * @param msg the detail message.
   */   
  public GreenPillUIError(String msg) {
    super(msg);
  }

}
