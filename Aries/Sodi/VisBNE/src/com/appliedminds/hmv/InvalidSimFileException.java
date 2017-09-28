package com.appliedminds.hmv;

import com.appliedminds.martini.MartiniException;


/**
 * This exception is thrown when a sim file contains invalid data.
 *
 * @author will@apmindsf.com
 */
public class InvalidSimFileException extends MartiniException {

  /**
   * Create an exception with a detail message.
   *
   * @param msg a description of the exception
   */
  public InvalidSimFileException(String msg)
  {
    super(msg);
  }

} // end class InvalidSimFileException
