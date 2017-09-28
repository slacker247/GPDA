package com.appliedminds.martinix.ui;

import java.awt.Font;
import java.awt.Graphics2D;
import java.awt.Shape;
import java.awt.font.LineBreakMeasurer;
import java.awt.font.TextAttribute;
import java.awt.font.TextHitInfo;
import java.awt.font.TextLayout;
import java.text.AttributedCharacterIterator.Attribute;
import java.text.AttributedCharacterIterator;
import java.text.AttributedString;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import com.appliedminds.martini.Size;


/**
 * This class supports a subset of TextLayout methods needed for drawing
 * multiline labels.
 *
 * @author will@apmindsf.com
 */
public class MultiLineTextLayout extends Size
{
  private int _maxWidth;
  private String _text;
  private AttributedString _attrStr;
  private LineBreakMeasurer _measurer;
  private ArrayList _lineData;
  private double _lineHeight;
  private int _maxIndex;
  private Font _font;


  public MultiLineTextLayout(int maxWidth, Graphics2D g, Font font,
                             String text)
  {
    _maxWidth = maxWidth;
    _lineData = new ArrayList();
    _font = font;
    initLineBreakMeasurer(g, text);
  }


  private void initLineBreakMeasurer(Graphics2D g, String text)
  {
    if (text.length() == 0)
      _text = " ";
    else
      _text = text;

    Map attrMap = new HashMap();
    attrMap.put(TextAttribute.FONT, _font);
    _attrStr = new AttributedString(_text, attrMap);

    AttributedCharacterIterator itr = _attrStr.getIterator();
    _lineData.clear();

    int start = itr.getBeginIndex();
    int end = itr.getEndIndex();

    _measurer = new LineBreakMeasurer(itr, g.getFontRenderContext());
    _measurer.setPosition(start);

    setHeight(0);
    setWidth(0);

    TextLayout layout  = null;
    while (_measurer.getPosition() < end) {
      int lineStart = _measurer.getPosition(); // start index of the line
      layout = _measurer.nextLayout(_maxWidth);
      if (layout.getAdvance() > getWidth()) {
        setWidth(layout.getAdvance());
      }

      setHeight(getHeight() + layout.getAscent() + layout.getDescent() +
                layout.getLeading());

      LineData ld = new LineData(lineStart,
                                 _measurer.getPosition()-1,
                                 layout);
      _lineData.add(ld);
    }

    // this is an average
    _lineHeight = getHeight() / _lineData.size();

    _maxIndex = ((LineData) _lineData.get(_lineData.size() - 1))._end;
  }


  public int getTextLines() {
    return _lineData.size();
  }


  public double getLineHeight() {
    return _lineHeight;
  }


  public int getMaxIndex() {
    return _maxIndex;
  }


  /**
   * @return the line number that contains the given index.
   */
  public int getLineNumber(int index) {
    if (index >= _maxIndex) {
      return (_lineData.size() - 1);
    }
    int res = 0;
    for (Iterator itr=_lineData.iterator(); itr.hasNext();res++) {
      LineData line = (LineData) itr.next();
      if ((index >= line._start) && (index <= line._end)) {
        break;
      }
    }

    return (res);
  }


  public Iterator getLineDataIterator() {
    return _lineData.iterator();
  }


  public LineBreakMeasurer getLineBreakMeasurer() {
    return _measurer;
  }


  public TextHitInfo getNextRightHit(int index) {
    LineData line = getLineDataByIndex(index);
    if (line != null) {
      int nextPos = index - line._start;

      if (nextPos == line._end) {
        // we need to go to the next line if there is any,
        // or not advance any more
        int lineNum = getLineNumber(nextPos);
        if (lineNum < (_lineData.size() - 1)) {
          return ((LineData)_lineData.get(lineNum+1))._layout.getNextLeftHit(1);
        }
        else {
          // last line
          int newPos = (nextPos > 0) ? (nextPos - 1) : nextPos;
          return (line._layout.getNextRightHit(newPos));
        }
      }
      return line._layout.getNextRightHit(index - line._start);
    }

    return null;
  }


  public TextHitInfo getNextLeftHit(int index) {
    LineData line = getLineDataByIndex(index);
    if (line != null) {
      return line._layout.getNextLeftHit(index - line._start);
    }

    return null;
  }


  public TextLayout getLayout(int index) {
    if (index > _maxIndex) {
      index = _maxIndex;
    }

    LineData line = getLineDataByIndex(index);
    if (line != null) {
      return line._layout;
    }

    return null;
  }


  /**
   * @param x x position of a point relative to the top left corner of the
   *          text block.
   * @param y y position of a point relative to the top left corner of the
   *          text block.
   * @return the index into the LineBreakMeasure
   */
  public int getInsertionIndex(int x, int y) {
    // estimate which line of text we're on
    int lineNum = (int) Math.floor(y / _lineHeight);
    int newY = (int) (y - (lineNum * _lineHeight));

    LineData line = (LineData) _lineData.get(lineNum);
    TextHitInfo info = line._layout.hitTestChar(x, newY);

    int res = line._start + info.getInsertionIndex();
    return (res);
  }


  /**
   * @return the start index of the line the given index is on.
   */
  public int getStartIndex(int idx) {
    LineData line = getLineDataByIndex(idx);
    if (line != null) {
      return line._start;
    }

    return 0;
  }


  public int findRightmostCaretIndex(int idx)
  {
    int lineNum = getLineNumber(idx);
    LineData line = getLineDataByIndex(idx);
    if (lineNum < (_lineData.size() - 1)) {
      return line._end;
    } else {
      return line._end+1;
    }
  }


  public Shape getLogicalHighlightShape(int start, int stop) {
    LineData lineStart = getLineDataByIndex(start);
    LineData lineStop = getLineDataByIndex(stop);

    if (lineStart == lineStop) {
      // on the same line. figure out the offset relative to the line
      int newStart = start - lineStart._start;
      int newStop = stop - lineStart._start;
      return (lineStart._layout.getLogicalHighlightShape(newStart, newStop));
    }
    else {
      int newStart = start - lineStart._start;
      int newStop = lineStart._end;
      return (lineStart._layout.getLogicalHighlightShape(newStart, newStop));
    }
  }


  public Shape getCaretShape(int index) {
    TextHitInfo hitInfo;

    if (index == 0) {
      hitInfo = getNextLeftHit(1);
    }
    else {
      hitInfo = getNextRightHit(index-1);
    }

    return (getLayout(index).getCaretShape(hitInfo));
  }


  private LineData getLineDataByIndex(int idx) {

    Iterator itr = _lineData.iterator();
    while (itr.hasNext()) {
      LineData line = (LineData) itr.next();
      if ((idx >= line._start) && (idx <= line._end)) {
        return line;
      }
    }

    if (idx > _maxIndex) {
      return ((LineData)_lineData.get(_lineData.size() - 1));
    }

    return null;
  }


  public class LineData
  {
    private int _start;
    private int _end;
    private TextLayout _layout;

    public LineData(int startIndex, int endIndex, TextLayout layout) {
      _start = startIndex;
      _end = endIndex;
      _layout = layout;
    }

    public TextLayout getTextLayout() {
      return _layout;
    }

  } // end private class LineData

} // end class MultiLineTextLayout
