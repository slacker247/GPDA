package com.appliedminds.martinix.slider;

import com.appliedminds.martini.MartiniIcon;
import java.net.URL;
import java.awt.TexturePaint;
import java.awt.geom.Rectangle2D;
import java.util.logging.Logger;


// test
import com.appliedminds.martinix.ui.DrawnObject;
import java.awt.*;


/**
 * Experimental pixmap code.
 */
class Tiles {


  private static final String CORNER_NW = 
    "com/appliedminds/martinix/slider/resources/corner_nw.png";

  private static final String CORNER_NE = 
    "com/appliedminds/martinix/slider/resources/corner_ne.png";

  private static final String CORNER_SW = 
    "com/appliedminds/martinix/slider/resources/corner_sw.png";

  private static final String CORNER_SE = 
    "com/appliedminds/martinix/slider/resources/corner_se.png";

  private static final String VERT_TILE_E = 
    "com/appliedminds/martinix/slider/resources/east.png";

  private static final String VERT_TILE_W = 
    "com/appliedminds/martinix/slider/resources/west.png";

  private static final String HORIZ_TILE_N = 
    "com/appliedminds/martinix/slider/resources/north.png";

  private static final String HORIZ_TILE_S = 
    "com/appliedminds/martinix/slider/resources/south.png";

  private static final String BG_TILE = 
    "com/appliedminds/martinix/slider/resources/bg.png";



  /*
   * The corner tiles must be exactly 24x24
   */
  public static final int CORNER_TILE_DIM = 24;  


  /*
   * The veritical tiles must be 24 pixels wide.
   */
  public static final int VERT_TILE_WIDTH = CORNER_TILE_DIM;


  /*
   * The horizontal tiles must be 24 pixels high.
   */
  public static final int HORIZ_TILE_HEIGHT = CORNER_TILE_DIM;


  /*
   * For all tiles, we draw them over the background starting on the
   * 13th pixel counting from the outside in.
   */
  public static final int CORNER_INSET = 12;
  public static final int CORNER_OVERLAY = 11;



    
  public MartiniIcon nwCorner;
  public MartiniIcon neCorner;
  public MartiniIcon swCorner;
  public MartiniIcon seCorner;
  public MartiniIcon vertTileE;
  public MartiniIcon vertTileW;
  public MartiniIcon horizTileN;
  public MartiniIcon horizTileS;
  public MartiniIcon bg;

  public TexturePaint bgPaint;

    
  public Tiles() throws Exception {
    Logger log = Logger.getAnonymousLogger();
    log.info("Loading pixmap tiles...");
    ClassLoader loader = getClass().getClassLoader();
    nwCorner = MartiniIcon.load(loader.getResource(CORNER_NW), null);
    log.info("nw:" + nwCorner);

    neCorner = MartiniIcon.load(loader.getResource(CORNER_NE), null);
    log.info("ne: " + neCorner);

    seCorner = MartiniIcon.load(loader.getResource(CORNER_SE), null);
    log.info("se: " + seCorner);

    swCorner = MartiniIcon.load(loader.getResource(CORNER_SW), null);
    log.info("sw: " + swCorner);

    vertTileE = MartiniIcon.load(loader.getResource(VERT_TILE_E), null);
    log.info(" e: " + vertTileE);

    vertTileW = MartiniIcon.load(loader.getResource(VERT_TILE_W), null);
    log.info(" w:" + vertTileW);

    horizTileN = MartiniIcon.load(loader.getResource(HORIZ_TILE_N), null);
    log.info(" n:" + horizTileN);

    horizTileS = MartiniIcon.load(loader.getResource(HORIZ_TILE_S), null);
    log.info(" s:" + horizTileS);

    bg = MartiniIcon.load(loader.getResource(BG_TILE), null);
    log.info("All pixmaps loaded.");

    bgPaint = 
      new TexturePaint(bg.getImage(), 
                       new Rectangle2D.Double(0, 
                                              0, 
                                              bg.getWidth(), 
                                              bg.getHeight()));     
  }


  /*
   * Adjust the width (of the background image area) so that it is an
   * even number of horiz-tiles wide.
   */
  public double roundWidth(double w) {

    int minLen = CORNER_TILE_DIM;

    //
    // Fix width
    //
    if (w < minLen) {
      w = (double) minLen;
    }
    else {
      int remain = ((int) w) - minLen;      

      if ((remain % horizTileN.getWidth()) != 0) {        
        int extra = horizTileN.getWidth() - (remain % horizTileN.getWidth());
        w += extra;
      }

      w -= 1.0;
    }

    return(w);
  }



  /*
   * Adjust the height (of the background image area) so that it is an
   * even number of vert-tiles wide.
   */
  public double roundHeight(double h) {

    int minLen = CORNER_TILE_DIM;


    //
    // Fix height
    //
    if (h < minLen) {
      h = (double) minLen;
    }
    else {
      int remain = ((int) h) - minLen;      
      if ((remain % vertTileE.getHeight()) != 0) {        
        int extra = vertTileE.getHeight() - (remain % vertTileE.getHeight());
        h += extra;
      }

      h -= 1.0;
    }


    return(h);
  }


  /*
   * Given a proposed width value for the node, return the best fit
   * width value that includes space for all pixmaps.
   */
  public double roundWidthTotal(double w) {
    return(roundWidth(w) + CORNER_TILE_DIM);
  }


  /*
   * Given a proposed height value for the node, return the best fit
   * height value that includes space for all pixmaps.
   */
  public double roundHeightTotal(double h) {
    return(roundHeight(h) + CORNER_TILE_DIM);
  }


  /**
   * Figure out the size and position of the background are within the
   * fullbounds.  This assumes VIEWPORT coordinates.
   *
   * @param fullBounds the full bounds of the pixmapped node in
   * viewport coordinates.
   *
   * @return a new rectangle object that is the bounds of just the
   * background area within the fullBounds (in viewport coordinates).
   */
  public static Rectangle2D computeBGBoundsVP(Rectangle2D fullBounds) {    
    Rectangle2D bgArea = 
      new Rectangle2D.Double(fullBounds.getX() + Tiles.CORNER_INSET,
                             fullBounds.getY() + Tiles.CORNER_INSET,
                             fullBounds.getWidth() - Tiles.CORNER_TILE_DIM,
                             fullBounds.getHeight() - Tiles.CORNER_TILE_DIM);
    return(bgArea);
  }



  /**
   * Figure out the size and position of the background are within the
   * fullbounds.  This assumes WORLD coordinates.
   *
   * @param fullBounds the full bounds of the pixmapped node in
   * world coordinates.
   *
   * @return a new rectangle object that is the bounds of just the
   * background area within the fullBounds (in world coordinates).
   */
  public static Rectangle2D computeBGBoundsW(Rectangle2D fullBounds) {    
    Rectangle2D bgArea = 
      new Rectangle2D.Double(fullBounds.getX() + Tiles.CORNER_INSET,
                             fullBounds.getY() - Tiles.CORNER_INSET,
                             fullBounds.getWidth() - Tiles.CORNER_TILE_DIM,
                             fullBounds.getHeight() - Tiles.CORNER_TILE_DIM);
    return(bgArea);
  }

 
}// end "Tiles"

