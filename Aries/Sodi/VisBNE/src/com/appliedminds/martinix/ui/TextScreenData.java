package com.appliedminds.martinix.ui;


import java.awt.Point;
import java.awt.Shape;
import java.awt.font.TextHitInfo;
import java.awt.font.TextLayout;
import java.awt.geom.Rectangle2D;
import java.text.AttributedCharacterIterator;
import java.text.AttributedString;



/**
 * This object holds information about text that has been drawn on the
 * screen.  There is a text layout object with information about the
 * font layout, and a bounds object with information about the screen
 * area and position of the text.
 *
 * <p>The reason that this object is needed is that the TextLayout
 * object provided by java does not include anything about position.
 *
 * <b>All coordinates and sizes are in screen (ie, viewport)
 * coordinates</b>
 *
 * <p>This object is immutable.
 *
 *
 *
 * @author mathias@apmindsf.com
 * @author ben@apmindsf.com
 * @author will@apmindsf.com
 */
public class TextScreenData {

  private boolean _singleLine;
  private TextLayout _singleLayout = null;
  private MultiLineTextLayout _multiLayout = null;
  private Rectangle2D _bounds;
  private AttributedString _attrStr;
  private String _text;


  /**
   * Create a new one with the text layout and postion information.
   *
   * @param layout the text layout object.
   * @param x the drawn X coordinate (on the screen).
   * @param y the drawn Y coordinate (on the screen).
   */
  public TextScreenData(TextLayout layout, int x, int y, String text) {
    _bounds = new Rectangle2D.Double(x,
                                     y,
                                     layout.getBounds().getWidth(),
                                     layout.getBounds().getHeight());
    _singleLayout = layout;
    _text = text;
    _singleLine = true;
  }


  /** 
   * Create a new one with the text layout and bounds information.
   *
   * @param layout the text layout object.
   * @param bounds the drawn bounds including POSITION.
   */
  public TextScreenData(TextLayout layout, Rectangle2D bounds, String text) {
    _bounds = bounds;
    _singleLayout = layout;
    _text = text;
    _singleLine = true;
  }


  public TextScreenData(MultiLineTextLayout layout, Rectangle2D bounds,
                        String text)
  {
    _multiLayout = layout;
    _bounds = bounds;
    _text = text;
    _attrStr = new AttributedString(_text);
    _singleLine = false;
  }


  /** 
   * Check if a point lies within our bounds.
   *
   * @return true if the point lies within our bounds.
   */
  public boolean hitTest(Point pt) {
    return(_bounds.contains(pt));
  }


  /** 
   * Get the text layout object.
   */
  public TextLayout getTextLayout() {
    if (_singleLine)
    {
      return(_singleLayout);
    }
    else
    {
      return (_multiLayout.getLayout(0));
    }
  }


  public TextLayout getTextLayout(int index) {
    if (_singleLine) {
      return _singleLayout;
    } else {
      return _multiLayout.getLayout(index);
    }
  }


  public MultiLineTextLayout getMultiLineTextLayout() {
    return _multiLayout;
  }


  /** 
   * Get the drawn bounds (in screen coordinates).
   */
  public Rectangle2D getBounds() {
    return(_bounds);
  }


  public Shape getCaretShape(int caretIndex) {
    if (_singleLine)
    {
      TextHitInfo hitInfo = findHitInfoByIndex(caretIndex);
      return (_singleLayout.getCaretShape(hitInfo));
    }
    else
    {
      return (_multiLayout.getCaretShape(caretIndex));
    }
  }


  public AttributedCharacterIterator getIterator() {
    if (_attrStr != null) {
      return _attrStr.getIterator();
    }

    return null;
  }

  
  /**
   * @return the leftmost caret index expressed as a character index.
   *
   * @see java.awt.font.TextHitInfo#getInsertionIndex()
   */
  public int findLeftmostCaretIndex() {
    return(0);
  }

  public int findLeftmostCaretIndex(int idx) {
    if (_singleLine) {
      return 0;
    }

    return _multiLayout.getStartIndex(idx);
  }


  /**
   * @return the appropriate caret index (character index).
   *
   * @see java.awt.font.TextHitInfo#getInsertionIndex()
   */
  public int findCaretIndexByMousePosition(Point p) {
    return(findCaretIndexByMousePosition(p.getX(), p.getY()));
  }
  


  /**
   * Move caret index one word to the left.
   *
   * @param caretIndex the current caret index
   *
   * @return the caret index positioned on the first character of the
   * current word, or of the previous word if the caret is alreay on
   * the first charater or on a space.
   * 
   */
  public int findLeftByWordCaretIndex(int caretIndex)
  {
    int nextCaretIndex = 0;

    int i;
    for (i = nextCaretIndex + 1;
         i < caretIndex;
         i++)
    {
      char c = _text.charAt(i - 1);
      if (c == ' ')
      {
        nextCaretIndex = i;
      }
    }

    return nextCaretIndex;
  }



  /**
   * Move the caret index one word to the right.
   *
   * @param caretIndex the initial caret index.
   * @return the caret index positioned on the space character at the
   * end of the current word.
   */
  public int findRightByWordCaretIndex(int caretIndex)
  {
    int nextCaretIndex = _text.length();

    for(int i= caretIndex + 1; i < _text.length(); ++i) {
      if (_text.charAt(i) == ' ') {
        return(i);
      }
    }

    // as a default, return the end of the text.
    return(_text.length());
  }



  /** 
   * Move the caret left by one character, if possible.
   *
   * @param caretIndex the current caret index.
   * @return the caret index positioned one character to the left.
   */
  public int findLeftByCharCaretIndex(int caretIndex) {
    int res = 0;
    if (_singleLine) {
      TextHitInfo nextHitInfo = _singleLayout.getNextLeftHit(caretIndex);
      if (nextHitInfo != null)
      {
        res = nextHitInfo.getInsertionIndex();
      }
    } else {
      res = caretIndex - 1;
      if (res < 0) {
        res = 0;
      }
    }

    return res;
  }



  /** 
   * Move the caret right by on character, if possible.
   *
   * @param caretIndex the current position.
   * @return the caret position one character to the right, or the end
   * of the text if there are no more characters.
   */
  public int findRightByCharCaretIndex(int caretIndex)
  {
    int res = 0;

    int nextCaretIndex = caretIndex;
    TextHitInfo nextHitInfo = null;
    if (_singleLine) {
      nextHitInfo = _singleLayout.getNextRightHit(caretIndex);
      if (nextHitInfo != null)
      {
        res = nextHitInfo.getInsertionIndex();
      }
    } else {
      if (caretIndex <= (_multiLayout.getMaxIndex())) {
        res = caretIndex + 1;
      }
      else {
        res = caretIndex;
      }
    }

    return res;
  }


  public int findRightmostCaretIndex()
  {
    return (findRightmostHitInfo(0).getInsertionIndex());
  }


  public int findRightmostCaretIndex(int idx) {
    if (_singleLine) {
      return (findRightmostHitInfo(0).getInsertionIndex());
    }
    else {
      return (_multiLayout.findRightmostCaretIndex(idx));
    }
  }


  /**
   * @return the Y viewport coordinate that corresponds to the line that
   *  contains the index.
   */
  public int getLineYByIndex(int index) {
    if (_singleLine) {
      return 0;
    }

    int lineNum = _multiLayout.getLineNumber(index);
    int y = (int)(_bounds.getY() + (lineNum * _multiLayout.getLineHeight()));
    return y;
  }


  public float getLineHeight() {
    if (_singleLine) {
      return _singleLayout.getAscent();
    } else {
      return (float) _multiLayout.getLineHeight();
    }
  }


  /**
   * currently only supports start and stop on the same line.
   *
   * @return a shape the corresponds to the start and stop highlight index
   */
  public Shape getLogicalHighlightShape(int start, int stop) {
    if (_singleLine) {
      return _singleLayout.getLogicalHighlightShape(start, stop);
    }
    else {
      return _multiLayout.getLogicalHighlightShape(start, stop);
    }
  }


  /* 
   * Returns a HitInfo object that will place the caret between the
   * characters string[index - 1] and string[index], (where index is
   * zero-based).
   */
  private TextHitInfo findHitInfoByIndex(int index)
  {
    if (index == 0)
    {
      if (_singleLine) {
        return _singleLayout.getNextLeftHit(1);
      } else {
        return _multiLayout.getNextLeftHit(1);
      }
    }
    else
    {
      if (_singleLine) {
        return _singleLayout.getNextRightHit(index - 1);
      } else {
        return _multiLayout.getNextRightHit(index - 1);
      }
    }
  }


  /**
   * Returns a HitInfo object that will place the caret to the right
   * of the entire string.
   */
  private TextHitInfo findRightmostHitInfo(int idx)
  {
    TextHitInfo next = findHitInfoByIndex(idx);
    int insertionIndex = next.getInsertionIndex();
    TextLayout layout = getTextLayout(idx);
    TextHitInfo nextMaybe;

    while ((nextMaybe = layout.getNextRightHit(insertionIndex)) != null)
    {
      next = nextMaybe;
      insertionIndex = next.getInsertionIndex();
    }
 
    return next;
  }



  /**
   *
   * @see java.awt.font.TextHitInfo#getInsertionIndex()
   */
  private int findCaretIndexByMousePosition(double x, double y)
  {
    // Safe return value
    int caretIndex = 0;

    // Mouse position args are specified in viewport coordinates
    TextHitInfo textHitInfo = null;
    if (_singleLine) {
      textHitInfo = _singleLayout.hitTestChar((float)(x - _bounds.getX()),
                                              (float)(y - _bounds.getY()));
      if (textHitInfo != null) {
        caretIndex = textHitInfo.getInsertionIndex();
      }
    } else {
      caretIndex = _multiLayout.getInsertionIndex((int)(x - _bounds.getX()),
                                                  (int)(y - _bounds.getY()));
    }

    return caretIndex;
  }

}// end class "TextScreenData"
