/* -*- Mode: Java; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*-*/
package com.appliedminds.martinix.gapp;


import java.util.EventListener;
import com.appliedminds.martini.GraphPanel;




/**
 * Those who wish to be notified when a new graph is displayed in the
 * GAppFrame should implement this interface.
 *
 * @author daepark@apmindsf.com
 */
public interface GAppFrameListener extends EventListener {

	/**
	 * Notify that a graph has been updated (i.e., loaded from a file,
	 * URL, etc.).
	 *
	 * @param graphPanel the updated GraphPanel
	 */
	void gAppFrameGraphUpdated(GraphPanel graphPanel);


} // end interface GraphUpdateListener
