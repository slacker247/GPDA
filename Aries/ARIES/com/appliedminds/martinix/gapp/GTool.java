/* -*- Mode: Java; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*-*/
package com.appliedminds.martinix.gapp;

import java.awt.Cursor;
import java.awt.Rectangle;
import java.awt.event.MouseEvent;
import java.awt.event.KeyEvent;
import java.awt.geom.Rectangle2D;

import com.appliedminds.martini.DrawableGraphMouseEvent;
import com.appliedminds.martini.DrawableGraphMultipleSelectionEvent;




/**
 */
public interface GTool {


	/**
	 * @return the Cursor that will represent this tool when this
	 * tool is activated.
	 */
	Cursor getCursor();


	/**
	 * Notification that the tool has been activated.
	 * Perferm any intialization steps before it's actually used.
   *
   * @args any arguments this tool need to be activated.
	 */
	void activate(Object[] args);


	/**
	 * Notification that the tool has been deactivated.
	 * Do any clean up before another tool is activated.
	 */
	void deactivate();


  /**
   * Handle a single mouse click event
   */
  void mouseClicked(MouseEvent e);

  /**
   * Handle a double mouse click event
   */
  void mouseDoubleClicked(MouseEvent e);


  /**
   * Handle a mouse pressed event.
   */
  void mousePressed(MouseEvent e);


  /**
   * Handle a mouse released event.
   */
  void mouseReleased(MouseEvent e);


  /**
   * Handle a mouse entered event.
   */
  void mouseEntered(MouseEvent e);


  /**
   * Handle a mouse exit event.
   */
  void mouseExited(MouseEvent e);

  
  /**
   * Handle a mouse dragged event.
   */
  void mouseDragged(MouseEvent e);


  /**
   * Handle a mouse moved event.
   */
  void mouseMoved(MouseEvent e);

  
  /**
   * Handle a key pressed event.
   */
  void keyPressed(KeyEvent e);


  /**
   * Handle a key released event.
   */
  void keyReleased(KeyEvent e);
  

  /**
   * Handle a key typed event.
   */
  void keyTyped(KeyEvent e);


  /**
   * Handle a Marquee Selection event.
   */
  void handleMarqueeSelection(Rectangle marqueeBounds);


} // end interface TMVTool
