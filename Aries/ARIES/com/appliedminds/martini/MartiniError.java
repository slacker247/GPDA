package com.appliedminds.martini;

/**
 * A general purpose run-time error.
 */
public class MartiniError extends RuntimeException {

  public MartiniError(String name) {
    super(name);
  }


} // end class MartiniError
