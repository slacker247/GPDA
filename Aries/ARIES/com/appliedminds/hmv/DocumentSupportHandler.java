package com.appliedminds.hmv;

import com.appliedminds.martini.DrawableGraphElement;

import java.net.URL;
import javax.swing.JFrame;
import java.awt.Point;
import java.util.logging.Logger;


/**
 * A URL document handler.
 *
 * @author daepark@apmindsf.com
 */
public class DocumentSupportHandler implements SupportHandler {

  private static final String P_MOZILLA  = "mozilla";
  private static final String P_EXPLORER = "explorer";

  private static final String DEF_MOZ_BIN = "/usr/local/mozilla/mozilla";
  private static final String DEF_IE_BIN = "explorer.exe";

  public void handleURL(DrawableGraphElement element,
                        URL url,
                        Point clickPoint)
  {
    Logger.getLogger(HMV.LOG_NAME).info(getClass().getName() + " handling url: " + url);

    try {
      //
      // Fix: mozilla dependency.
      //
      String mozBin = DEF_MOZ_BIN;

      boolean ie =
        "true".equals(System.getProperty(P_EXPLORER)) ? true : false;

      if (ie) {
        mozBin = DEF_IE_BIN + " " + url.toString();
      }
      else {
        String moz = System.getProperty(P_MOZILLA);

        if (moz == null) {
          moz = DEF_MOZ_BIN;
        }

        mozBin = moz + " -remote openURL(" + url.toString() + ")";
      }

      Logger.getLogger(HMV.LOG_NAME).info(mozBin);
      Runtime.getRuntime().exec(mozBin);
    }
    catch (Exception e) {
      e.printStackTrace();
    }
  }

} // end interface "DocumentSupportHandler"
