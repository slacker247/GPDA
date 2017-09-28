package com.appliedminds.martinix.ui;

import java.awt.Graphics2D;
import java.awt.Paint;
import java.awt.Composite;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.awt.font.TextLayout;

/**
 * A DrawnObject implementation for a drawn (single-line) TextScreenData.
 *
 * @author daepark@apmindsf.com
 */
public class DrawnText extends DrawnObject {

  protected TextScreenData _data;
  protected Paint _fontPaint;
  private float _x;
  private float _y;


  /**
   * Initialize with params need for redrawing a TextScreenData.
   *
   * @param data the TextScreenData containing the multi-line TextLayout.
   * @param fontPaint the paint used to paint the text. If null, nothing
   * will be redrawn.
   * @param center The centered position of the drawn text.
   */
  public DrawnText(TextScreenData data, Paint fontPaint, Point2D center)
  {
    super();
    _data = data;
    _fontPaint = fontPaint;

    // calculate the draw position of the layout
    TextLayout layout = _data.getTextLayout();
    _x = (float)(center.getX() - layout.getBounds().getWidth() / 2);
    _y = (float)(center.getY() + layout.getAscent() / 2);
  }

  public DrawnText(TextScreenData data, Paint fontPaint,
                   Point2D center, Composite composite)
  {
    super(composite);
    _data = data;
    _fontPaint = fontPaint;

    // calculate the draw position of the layout
    TextLayout layout = _data.getTextLayout();
    _x = (float)(center.getX() - layout.getBounds().getWidth() / 2);
    _y = (float)(center.getY() + layout.getAscent() / 2);
  }


  /**
   * Initialize with params need for redrawing a TextScreenData.
   *
   * @param data the TextScreenData containing the TextLayout
   * @param fontPaint the paint used to paint the text. If null, nothing
   * will be redrawn.
   * @param x The origin of the text layout is placed at x, y.
   * @param y The origin of the text layout is placed at x, y.
   */
  public DrawnText(TextScreenData data, Paint fontPaint, float x, float y)
  {
    super();
    _data = data;
    _fontPaint = fontPaint;

    // calculate the draw position of the layout
    _x = x;
    _y = y;
  }

  public DrawnText(TextScreenData data, Paint fontPaint,
                   float x, float y, Composite composite)
  {
    super(composite);
    _data = data;
    _fontPaint = fontPaint;

    // calculate the draw position of the layout
    _x = x;
    _y = y;
  }


  /**
   * @return the drawn TextScreenData.
   */
  public TextScreenData getDrawnTextScreenData() {
    return (_data);
  }


  //
  // begin DrawnObject interface
  //

  public void redraw(Graphics2D graphics) {
    super.redraw(graphics);

    if (_fontPaint != null) {
      graphics.setPaint(_fontPaint);
      _data.getTextLayout().draw(graphics, _x, _y);
    }
  }

  //
  // end DrawnObject interface
  //

} //end class DrawnText
