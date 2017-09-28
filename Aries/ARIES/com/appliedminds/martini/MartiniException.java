package com.appliedminds.martini;


import java.io.Writer;
import java.io.StringWriter;
import java.io.PrintWriter;



/**
 * The top-level Martini exception class.  Supports nesting.
 *
 * @author mathias@sf.appliedminds.net
 */
public class MartiniException extends Exception {


  private String _msg;
  private String _nestedTrace;
  private Throwable _nested;


  /**
   * Create an exception with a detail message.
   *
   * @param msg a description of the exception
   */
  public MartiniException(String msg) {
    _msg = msg;
    _nested = null;
    _nestedTrace = null;
  }


  /** 
   * Create an exception with a nested exception and a 
   * detail message.
   *
   * @param msg the detail message.
   * @param nested some nested exception.
   */
  public MartiniException(String msg, Throwable nested) {
    _msg = msg;
    _nested = nested;
    StringWriter buf = new StringWriter();
    PrintWriter out = new PrintWriter(buf);
    nested.printStackTrace(out);
    out.flush();
    _nestedTrace = buf.toString();    
    out.close();    
  }


  /**
   * Get a string representation of this exception, including details
   * about the nested exception if any.
   */
  public String toString() {
    StringBuffer buf = new StringBuffer();
    buf.append(getClass().getName()).append(": ").append(_msg);
    if (_nestedTrace != null) {
      buf.append("\n------ nested exception ------\n");
      buf.append(_nestedTrace);
    }
    return(buf.toString());
  }


  /** 
   * Get the message associated with this exception (and also the one with
   * the nested exception, if any).
   */
  public String getMessage() {
    StringBuffer buf = new StringBuffer();
    buf.append(_msg);
    if (_nested != null) {
      buf.append(" (nested: ").append(_nested.getMessage());
    }
    return(buf.toString());
  }    


  /** 
   * Handy method to print a stack trace to a Writer.
   *
   * @param w the writer to print the stack trace to.
   */
  public void printStackTrace(Writer w) {
    PrintWriter out = new PrintWriter(w);
    super.printStackTrace(out);
    out.flush();
    out = null;
  }


}//end class
