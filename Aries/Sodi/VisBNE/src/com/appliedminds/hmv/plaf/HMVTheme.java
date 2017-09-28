package com.appliedminds.hmv.plaf;

import java.awt.Font;
import javax.swing.plaf.*;
import javax.swing.plaf.metal.*;

import com.appliedminds.martinix.ui.FontCache;

/**
 * Changes some of the visual properties of the metal theme
 *
 * @author darin@apmindsf.com, daepark@apmindsf.com
 */
public class HMVTheme extends DefaultMetalTheme {

  private static final String ARIAL_PLAIN_PATH =
    "com/appliedminds/hmv/plaf/fonts/arial.ttf";
  private static final String ARIAL_BOLD_PATH =
    "com/appliedminds/hmv/plaf/fonts/arialbd.ttf";

  private static final int NORMAL_SIZE = 12;
  private static final int SMALL_SIZE = 10;

  private static FontCache standardPlainCache =
    new FontCache(ARIAL_PLAIN_PATH);
  private static FontCache standardBoldCache =
    new FontCache(ARIAL_BOLD_PATH);
  private static FontCache titlebarBoldCache =
    new FontCache(ARIAL_BOLD_PATH);


  /**
   * @return the human-readable name of this theme
   */
  public String getName()
  {
    return "HMVTheme";
  }


  // font methods

  public FontUIResource getControlTextFont()
  {
    return new FontUIResource(standardPlainCache.getFontBySize(NORMAL_SIZE));
  }

  public FontUIResource getMenuTextFont()
  {
    return new FontUIResource(standardPlainCache.getFontBySize(NORMAL_SIZE));
  }

  public FontUIResource getSubTextFont()
  {
    return new FontUIResource(standardPlainCache.getFontBySize(SMALL_SIZE));
  }

  public FontUIResource getSystemTextFont()
  {
    return new FontUIResource(standardPlainCache.getFontBySize(NORMAL_SIZE));
  }

  public FontUIResource getUserTextFont()
  {
    return new FontUIResource(standardPlainCache.getFontBySize(NORMAL_SIZE));
  }

  public FontUIResource getWindowTitleFont()
  {
    return new FontUIResource(standardBoldCache.getFontBySize(NORMAL_SIZE));
  }


  /**
   * @return the system text font used by this theme (used by AA app - not
   * a normal part of a Metal theme extension)
   */
  public static Font getSystemFont()
  {
    return standardPlainCache.getFontBySize(NORMAL_SIZE);
  }


  /**
   * @return a bold version of the system text font used by this theme (used
   * by AA app - not a normal part of a Metal theme extension)
   */
  public static Font getSystemBoldFont()
  {
    return standardBoldCache.getFontBySize(NORMAL_SIZE);
  }


  /**
   * @return the small text font used by this theme (used by AA app - not
   * a normal part of a Metal theme extension)
   */
  public static Font getSmallFont()
  {
    return standardPlainCache.getFontBySize(SMALL_SIZE);
  }


  /**
   * @return the Font used in Titlebars
   */
  public static Font getTitlebarFont()
  {
    return titlebarBoldCache.getFontBySize(NORMAL_SIZE);
  }

  private final ColorUIResource secondary1 = new ColorUIResource(126, 124, 125);
  private final ColorUIResource secondary2 = new ColorUIResource(177, 175, 176);
  private final ColorUIResource secondary3 = new ColorUIResource(228, 226, 227);
  private static ColorUIResource toolBarBackground = new ColorUIResource(238, 235, 231);
  private static ColorUIResource scrollBarBackground = new ColorUIResource(247, 247, 247);
  private static ColorUIResource scrollBarForeground = new ColorUIResource(220, 220, 220);
//   public ColorUIResource getMenuBackground() {
//     return (toolBarBackground);
//   }

  protected ColorUIResource getSecondary1() { return secondary1; }
  protected ColorUIResource getSecondary2() { return secondary2; }
  protected ColorUIResource getSecondary3() { return secondary3; }
  protected ColorUIResource getToolBarBackground() { return toolBarBackground; }
  protected ColorUIResource getScrollBarBackground() { return scrollBarBackground; }
  protected ColorUIResource getScrollBarForeground() { return scrollBarForeground; }

} // end class
