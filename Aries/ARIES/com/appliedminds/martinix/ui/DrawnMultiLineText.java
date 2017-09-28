package com.appliedminds.martinix.ui;

import java.awt.Graphics2D;
import java.awt.Paint;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.awt.font.TextLayout;
import java.util.Iterator;

/**
 * A DrawnObject implementation for a drawn (multi-line) TextScreenData.
 *
 * @author will@apmindsf.com
 * @author daepark@apmindsf.com
 */
public class DrawnMultiLineText extends DrawnText {

  private Point2D _center;
  private boolean _centered = false;

  public DrawnMultiLineText(TextScreenData data,
                            Paint fontPaint,
                            Point2D center)
  {
    super(data, fontPaint, center);
    _center = center;
    _centered = false;
  }


  public DrawnMultiLineText(TextScreenData data,
                            Paint fontPaint,
                            Point2D center,
                            boolean centered)
  {
    super(data, fontPaint, center);
    _center = center;
    _centered = centered;
  }


  /**
   * Override DrawnText.redraw()
   */
  public void redraw(Graphics2D graphics) {
    if (_fontPaint != null) {
      Rectangle2D bounds = _data.getBounds();
      graphics.setPaint(_fontPaint);

      // starting positions
      float drawPosY = (float)(_center.getY() - (bounds.getHeight() / 2));
      float drawPosLeft = (float)(_center.getX() - (bounds.getWidth() / 2));

      float lineHeight = (float) _data.getMultiLineTextLayout().getLineHeight();

      Iterator itr = _data.getMultiLineTextLayout().getLineDataIterator();
      while (itr.hasNext()) {
        TextLayout layout = ((MultiLineTextLayout.LineData) itr.next()).getTextLayout();


        float drawPosX = drawPosLeft;
        if (_centered) {
          float dx =
            (float)(bounds.getWidth() - layout.getBounds().getWidth());
          drawPosX = drawPosLeft + dx / 2;
        }

        drawPosY += layout.getAscent();

        layout.draw(graphics, drawPosX, drawPosY);

        drawPosY += layout.getDescent() + layout.getLeading();
      }
    }
  }

} // end class DrawnMultiLineText
