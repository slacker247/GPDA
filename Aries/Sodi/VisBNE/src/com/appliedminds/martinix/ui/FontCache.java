package com.appliedminds.martinix.ui;


import com.appliedminds.martini.MartiniError;
import java.awt.Font;
import java.io.InputStream;
import java.io.IOException;
import java.awt.FontFormatException;
import java.util.HashMap;
import java.net.URL;




/** 
 * The FontCache is responsible for loading fonts using javas resource
 * mechanism, and then for creating version with different point sizes
 * and caching them.
 *
 *
 *
 * @author mathias@apmindsf.com
 * @author daepark@apmindsf.com
 */
public class FontCache {

  private Font onePointFont;
  private HashMap fontsBySize;



  /**
   * Initialize the font cache with a resource path to a (presumably
   * true type) font.  For example, <code>com/package/blah.ttf</code>.
   *
   * @param resourcePath a resource path to a true-type font.
   * @throws MartiniError if some loading error occurs.
   */
  public FontCache(String resourcePath) throws MartiniError {

    // FIX: Should pass a URL or at least a valid URL string to 
    // this method.

    fontsBySize = new HashMap();
    InputStream is = null;

    try
    {
      // Trick to find font file in applet jar.
      // This duplicates log in IconAndCursorLoader.
      ClassLoader cl;
      URL url = null;

      cl = getClass().getClassLoader();
      url = cl.getResource(resourcePath);
      is = url.openStream();

      // This creates a 1 point font:
      onePointFont = Font.createFont(Font.TRUETYPE_FONT, is);

      // All other point sizes are derived later.
      fontsBySize.put(new Integer(1), onePointFont);
    }
    catch (IllegalArgumentException e)
    {
      throw(new MartiniError("Bad font resource path: " + 
                               e.toString()));
    }
    catch (FontFormatException e)
    {
      throw(new MartiniError("Font format error: " + 
                               e.toString()));
    }
    catch (IOException e)
    {
      throw(new MartiniError("I/O Error loading font: " +
                             e.toString()));
    }
    finally
    {
      try
      {
        is.close();
      }
      catch (IOException e) { }
    }
  }



  /** 
   * Get a font by size.
   *
   * @param size the desired point size.
   */
  public Font getFontBySize(int size)
  {
    Integer skey = new Integer(size);

    Font font = (Font)fontsBySize.get(skey);
    if (font == null)
    {
      font = onePointFont.deriveFont((float)size);
      fontsBySize.put(skey, font);
    }

    return font;
  }
}
