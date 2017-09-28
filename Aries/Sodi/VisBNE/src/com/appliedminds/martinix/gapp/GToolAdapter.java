package com.appliedminds.martinix.gapp;

import java.awt.Cursor;
import java.awt.Rectangle;
import java.awt.event.MouseEvent;
import java.awt.event.KeyEvent;


/** 
 * A GTool implementation that does nothing except return its
 * cursor.
 *
 *
 * @author mathias@apmindsf.com
 */
public class GToolAdapter implements GTool {


  private Cursor _cursor;


  public GToolAdapter(Cursor c) {
    _cursor = c;
  }

  public Cursor getCursor() {
    return(_cursor);
  }

  public void activate(Object[] args) { }

  public void deactivate() { }

  public void mouseClicked(MouseEvent e) { }

  public void mouseDoubleClicked(MouseEvent e) { }

  public void mousePressed(MouseEvent e) { }

  public void mouseReleased(MouseEvent e) { }

  public void mouseEntered(MouseEvent e) { }

  public void mouseExited(MouseEvent e) { }

  public void mouseDragged(MouseEvent e) { }

  public void mouseMoved(MouseEvent e) { }

  public void keyPressed(KeyEvent e) { }

  public void keyReleased(KeyEvent e) { }

  public void keyTyped(KeyEvent e) { }

  public void handleMarqueeSelection(Rectangle marqueeBounds) { }

} // end class GToolAdapter
