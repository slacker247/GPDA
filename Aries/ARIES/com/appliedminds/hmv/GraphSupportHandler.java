package com.appliedminds.hmv;

import com.appliedminds.hmv.dsg.DSGViewer;
import com.appliedminds.martini.*;
import com.appliedminds.martini.io.*;
import com.appliedminds.martinix.gapp.GActions;

import java.awt.Point;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.IOException;
import java.io.File;
import java.io.FileOutputStream;
import java.io.BufferedOutputStream;
import java.net.URL;
import java.util.logging.Logger;


/**
 * A handler that knows how to view a gml file specified by a URL.
 *
 * @author daepark@apmindsf.com
 */
public class GraphSupportHandler implements SupportHandler {


  public void handleURL(DrawableGraphElement element,
                        final URL url,
                        Point clickPoint)
  {
    Logger.getLogger(HMV.LOG_NAME).info(getClass().getName() + " handling url: " + url);

    final DSGViewer viewer = new DSGViewer();
    final DialogHandler viewerHandler = new DialogHandlerAdapter() {
        public void clickedOk() {
          //
          // For now, always save graph as a new file (by
          // timestamp) in the user's hmv home directory
          //
          File file = new File(url.getFile());
          if (file != null) {
            DrawableGraph g =
              viewer.getGraphPanel().getDrawableGraph();
            DrawableGraphContext ctx =
              viewer.getGraphPanel().getDrawableGraphContext();
            BufferedOutputStream out = null;
            try {
              out = new BufferedOutputStream(new FileOutputStream(file));
              GMLOutput.writeGML(g, out,
                                 GMLOutput.ROYERE_HACK, ctx);
            }
            catch(IOException err) {
              err.printStackTrace();
            }
            finally {
              try {
                out.flush();
                out.close();
              }
              catch(Exception ignore) { }
            }
          }
          viewer.setVisible(false);
          //viewer = null;
        }

        public void clickedCancel() {
          viewer.setVisible(false);
          //viewer = null;
        }
      };

    viewer.addDialogHandler(viewerHandler);
    viewer.setBounds(clickPoint.x, clickPoint.y,
                     700, 350);

    viewer.addWindowListener(new WindowAdapter() {
        public void windowClosing(WindowEvent e) {
          viewerHandler.clickedCancel();
        }
      });
    viewer.setVisible(true);
    viewer.requestFocus();

    viewer.loadFile(url.getFile());
  }


} // end class "GraphSupportHandler"
