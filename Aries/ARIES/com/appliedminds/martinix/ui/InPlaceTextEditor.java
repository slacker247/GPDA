/* -*- Mode: Java; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
package com.appliedminds.martinix.ui;

import java.awt.*;
import java.awt.event.*;
import java.awt.font.*;
import java.awt.geom.*;
import java.util.ArrayList;
import java.util.List;
import java.util.ListIterator;
import javax.swing.*;


/**
 * A port (and refactoring) of com.appliedminds.vge.gui.InPlaceTextEditor.
 *
 * This classes responsibilites are:
 * <ul>
 *    <li>Manage carat position.
 *    <li>Determine what a key press means.  Eg, DEL means ersase.
 *    <li>Determines if the editing session is over.
 * </ul>
 *
 *
 * @author ben@sf.appliedminds.net
 * @author mat@sf.appliedminds.net
 */
public class InPlaceTextEditor {

  // Return values for keyPressed()
  public static final int TEXTEDIT_CONTINUE = 0;
  public static final int TEXTEDIT_CANCEL   = 1;
  public static final int TEXTEDIT_COMPLETE = 2;


  private int _caretIndex;
  private int _nextCaretIndex;


  // FIX: Why do we need a manager?  Why isn't the selection its own
  //      manager?
  private SelectionManager _selectionManager;
  private TextSelection _selection;
  private TextSelection _nextSelection;

  private String _originalText;
  private StringBuffer _editedText;

  private Object _target;

  private TextScreenData _sdata;



  private boolean _modified = false;




  /**
   * @param clickPoint the first click point (used to determine the
   * initial caret position).  Can be null.
   */
  public InPlaceTextEditor(Object target,
                           String originalText,
                           TextScreenData sdata,
                           Point clickPoint)
  {

    if (clickPoint == null) {
      _caretIndex = sdata.findLeftmostCaretIndex();
    }
    else {
      _caretIndex = sdata.findCaretIndexByMousePosition(clickPoint);
    }

    _nextCaretIndex = _caretIndex;
    _target = target;

    _originalText = originalText;
    _editedText = new StringBuffer(originalText);


    // FIX: Need a way to keep this object up to date.
    _sdata = sdata;

    _selectionManager = new SelectionManager(_caretIndex);
    _selection = _selectionManager.getSelection();
    _nextSelection = _selectionManager.getSelection();

  }


  public void updateTextScreenData(TextScreenData data) {
    _sdata = data;
  }


  /**
   * Get the target of text editing operations.  This can be any
   * object and it is held as a service to the caller only-- the
   * object is never accessed within this class.
   *
   * @return the target object passed into the constructor.
   */
  public Object getTarget() {
    return(_target);
  }


  /**
   * Get the current text buffer.
   */
  public String getText()
  {
    return _editedText.toString();
  }


  /**
   * @return the text before start of edit.
   */
  public String getOriginalText() {
    return _originalText;
  }


  /**
   * Get the caret index.
   */
  public int getCaretIndex() {
    return(_caretIndex);
  }


  public int mouseEvent(MouseEvent e) {
    _modified = false;

    int result = TEXTEDIT_CONTINUE;

    boolean inBounds = _sdata.hitTest(e.getPoint());
    int mid = e.getID();

    if (inBounds) {

      int index = _sdata.findCaretIndexByMousePosition(e.getPoint());

      if (mid == MouseEvent.MOUSE_PRESSED) {
        // could be a start of a drag.
        // store location, but otherwise NOP.
        _nextCaretIndex = index;
        _nextSelection = _selectionManager.movePivotIndex(index);
      }
      else if (mid == MouseEvent.MOUSE_RELEASED) {
        // stop drag if one is in progress.
        // update selection area.
        _nextCaretIndex = index;
        _nextSelection = _selectionManager.extendSelection(index);
      }
      else if (mid == MouseEvent.MOUSE_DRAGGED) {
        // update selection area
        _nextCaretIndex = index;
        _nextSelection = _selectionManager.extendSelection(index);
      }
      else if ((mid == MouseEvent.MOUSE_CLICKED) &&
               (e.getClickCount() == 1))
      {
        // zero selection area.
        // update carat position
        _nextCaretIndex = index;
        _nextSelection = _selectionManager.collapseSelection();
      }
      else if ((mid == MouseEvent.MOUSE_CLICKED) &&
               (e.getClickCount() == 2))
      {
        // for now just SELECT ALL.
        selectAll();
      }
    }
    else {
      // Event occured outside our boundry.

      // zero the selection area if its a drag.
      if (mid == MouseEvent.MOUSE_DRAGGED) {
        _nextSelection = _selectionManager.collapseSelection();
      }

      // signal end of EDIT if a click.
      if (mid == MouseEvent.MOUSE_CLICKED) {
        result = TEXTEDIT_CANCEL;
      }
    }

    updateSelection();
    updateCaret();

    return(result);
  }


  public int keyPressed(KeyEvent e) {
    _modified = false;
    int result = TEXTEDIT_CONTINUE;

    // See VK_LEFT and VK_RIGHT
    boolean bCollapseSelection = false;

    switch(e.getKeyCode()) {

    case KeyEvent.VK_LEFT:
      if (e.isControlDown()) {
        // move by word
        _nextCaretIndex = _sdata.findLeftByWordCaretIndex(_caretIndex);
      }
      else {
        // move by letter
        _nextCaretIndex = _sdata.findLeftByCharCaretIndex(_caretIndex);
      }

      // any attempt to move left while caret is at the
      // absolute left of text means "drop selection".
      if ((_nextCaretIndex == _caretIndex) &&
          (_nextCaretIndex == _sdata.findLeftmostCaretIndex(_caretIndex)))
      {
        // note that selection may already be empty
        bCollapseSelection = true;
      }
      break;


    case KeyEvent.VK_RIGHT:
      if (e.isControlDown()) {
        // move by word
        _nextCaretIndex = _sdata.findRightByWordCaretIndex(_caretIndex);
      }
      else {
        // move by letter
        _nextCaretIndex = _sdata.findRightByCharCaretIndex(_caretIndex);
      }

      // any attempt to move right while caret is at the
      // absolute right of text means "drop selection".
      if ((_nextCaretIndex == _caretIndex) &&
          (_nextCaretIndex == _sdata.findRightmostCaretIndex(_caretIndex)))
      {
        // note that selection may already be empty
        bCollapseSelection = true;
      }
      break;

    case KeyEvent.VK_HOME:
      _nextCaretIndex = _sdata.findLeftmostCaretIndex(_caretIndex);

      // any attempt to move left while caret is at the
      // absolute left of text means "drop selection".
      if ((_nextCaretIndex == _caretIndex) &&
          (_nextCaretIndex == _sdata.findLeftmostCaretIndex(_caretIndex)))
      {
        // note that selection may already be empty
        bCollapseSelection = true;
      }
      break;

    case KeyEvent.VK_END:
      _nextCaretIndex = _sdata.findRightmostCaretIndex(_caretIndex);

      // any attempt to move right while caret is at the
      // absolute right of text means "drop selection".
      if ((_nextCaretIndex == _caretIndex) &&
          (_nextCaretIndex == _sdata.findRightmostCaretIndex(_caretIndex)))
      {
        // note that selection may already be empty
        bCollapseSelection = true;
      }
      break;

    case KeyEvent.VK_BACK_SPACE:
      // Delete selection, or delete character to the left of caret
      if (!_selection.isEmpty()) {
        deleteSelection();
      }
      else {
        deleteBeforeCaret();
      }
      break;

    case KeyEvent.VK_DELETE:
      // Delete selection, or delete character to the right of caret
      if (!_selection.isEmpty()) {
        deleteSelection();
      }
      else {
        deleteAfterCaret();
      }
      break;

    case KeyEvent.VK_ESCAPE:
      // Tell caller to abandon editing
      result = TEXTEDIT_CANCEL;
      break;

    case KeyEvent.VK_ENTER:
      // Tell caller we're done editing
      result = TEXTEDIT_COMPLETE;
      break;

    default:
      // Insert printable character in buffer
      char c = e.getKeyChar();
      if (c != KeyEvent.CHAR_UNDEFINED) {
        // if anything is selected, delete it first
        if (!_selection.isEmpty()) {
          deleteSelection();
        }

        // then insert new char
        insertBeforeCaret(c);
      }
      break;
    }

    // if caret moves...
    if (_nextCaretIndex != _caretIndex) {
      if (e.isShiftDown()) {
        _nextSelection = _selectionManager.extendSelection(_nextCaretIndex);
      }
      else {
        _nextSelection = _selectionManager.movePivotIndex(_nextCaretIndex);
      }

      updateCaret();
    }
    else {
      if (!e.isShiftDown()) {
        if (bCollapseSelection) {
          deselectAll();
        }
      }
    }

    return result;
  }


  /**
   * @return true if the selection is active.
   */
  public boolean hasSelection() {
    return(! _selection.isEmpty());
  }


  public int getSelectionStartIndex() {
    return(_selection.getStartIndex());
  }


  public int getSelectionStopIndex() {
    return(_selection.getStopIndex());
  }


  public boolean isModified() {
    return(_modified);
  }


  private void selectAll() {
    // Extend selection left
    _nextCaretIndex = _sdata.findLeftmostCaretIndex();
    _nextSelection = _selectionManager.extendSelection(_nextCaretIndex);
    updateCaret();

    // Extend selection right
    _nextCaretIndex = _sdata.findRightmostCaretIndex();
    _nextSelection = _selectionManager.extendSelection(_nextCaretIndex);
    updateCaret();
  }



  private synchronized void insertBeforeCaret(char c) {
    insertCharAt(_caretIndex, c);

    _nextCaretIndex = _caretIndex + 1;
    _nextSelection = _selectionManager.movePivotIndex(_nextCaretIndex);
    updateSelection();

    // New HitTest object ?

    _caretIndex = _nextCaretIndex;
  }


  private synchronized void deleteBeforeCaret() {
    if (_caretIndex > 0) {
      deleteCharAt(_caretIndex - 1);

      _nextCaretIndex = _caretIndex - 1;
      _nextSelection = _selectionManager.movePivotIndex(_nextCaretIndex);
      updateSelection();

      // New HitTest object ???

      _caretIndex = _nextCaretIndex;
    }
  }


  private synchronized void deleteAfterCaret() {
    if (_caretIndex < _editedText.length()) {
      deleteCharAt(_caretIndex);
      // New HitTest object ???
    }
  }


  private synchronized void deleteSelection()
  {
    deleteCharRange(_selection.getStartIndex(), _selection.getStopIndex());

    _nextCaretIndex = _selection.getStartIndex();
    _nextSelection = _selectionManager.movePivotIndex(_nextCaretIndex);
    updateSelection();

    // New HitTest object ???

    _caretIndex = _nextCaretIndex;
  }


  private synchronized void deselectAll()
  {
    _nextSelection = _selectionManager.collapseSelection();
    updateSelection();
  }



  /**
   * ASSUMES THAT CARET HAS BEEN UNDRAWN BY THE CALLER!
   */
  private synchronized void updateSelection()
  {
    // There may not be any text selection changes to handle.
    if (!_selection.equals(_nextSelection)) {
      _selection = _nextSelection;
      _modified = true;
    }
  }



  /*
   * Handle changes in the caret position
   */
  private synchronized void updateCaret() {
    if (_caretIndex != _nextCaretIndex) {
      // Handle changes in the text selection
      updateSelection();
      _caretIndex = _nextCaretIndex;
      _modified = true;
    }
  }



  private void insertCharAt(int index, char c)
  {
    _editedText.insert(index, c);
    _modified = true;
  }


  private void deleteCharAt(int index)
  {
    _editedText.deleteCharAt(index);
    _modified = true;
  }


  private void deleteCharRange(int index1, int index2)
  {
    _editedText.delete(index1, index2);
    _modified = true;
  }
}// end "InPlaceTextEditor"




/**
 * SelectionManager
 * ----------------
 *
 */
class SelectionManager
{
  private TextSelection sel;

  SelectionManager(int index)
  {
    sel = new TextSelection(index, index);
  }

  // always returns a copy
  public TextSelection getSelection()
  {
    return new TextSelection(sel);
  }

  private int getPivotIndex()
  {
    return sel.getPivotIndex();
  }

  private int getStartIndex()
  {
    return sel.getStartIndex();
  }

  private int getStopIndex()
  {
    return sel.getStopIndex();
  }

  // Parameter "index" is new caret position
  public TextSelection extendSelection(int index)
  {
    if (index > getPivotIndex())
    {
      setStopIndex(index);
    }
    else if (index < getPivotIndex())
    {
      setStartIndex(index);
    }
    else
    {
      setStartIndex(index);
      setStopIndex(index);
    }

    return getSelection();
  }

  public TextSelection collapseSelection()
  {
    setStartIndex(getPivotIndex());
    setStopIndex(getPivotIndex());
    return getSelection();
  }

  public TextSelection movePivotIndex(int index)
  {
    sel.setPivotIndex(index);
    return collapseSelection();
  }

  private void setPivotIndex(int index)
  {
    sel.setPivotIndex(index);
  }

  private void setStartIndex(int index)
  {
    sel.setStartIndex(index);
  }

  private void setStopIndex(int index)
  {
    sel.setStopIndex(index);
  }
}//end class "SelectionManager"


/**
 * TestSelection
 * -------------
 *
 */
class TextSelection
{
  // startIndex <= pivotIndex <= stopIndex
  private int pivotIndex;
  private int startIndex;
  private int stopIndex;

  public TextSelection(int pivotIndex) {
    setPivotIndex(pivotIndex);
    setStartIndex(pivotIndex);
    setStopIndex(pivotIndex);
  }

  public TextSelection(int startIndex, int stopIndex) {
    setPivotIndex(startIndex);
    setStartIndex(startIndex);
    setStopIndex(stopIndex);
  }

  public TextSelection(TextSelection ts) {
    setPivotIndex(ts.getPivotIndex());
    setStartIndex(ts.getStartIndex());
    setStopIndex(ts.getStopIndex());
  }

  public int getPivotIndex() {
    return pivotIndex;
  }

  public int getStartIndex() {
    return startIndex;
  }

  public int getStopIndex() {
    return stopIndex;
  }

  public void setPivotIndex(int pivotIndex) {
    this.pivotIndex = pivotIndex;
  }

  public void setStartIndex(int startIndex) {
    this.startIndex = startIndex;
  }

  public void setStopIndex(int stopIndex) {
    this.stopIndex = stopIndex;
  }

  public boolean equals(Object object) {
    boolean result = false;

    if (object != null) {
      TextSelection ts = (TextSelection)object;
      result = ((ts.getStartIndex() == getStartIndex()) &&
                (ts.getStopIndex() == getStopIndex()));
    }

    return result;
  }

  public boolean isEmpty() {
    return (getStartIndex() == getStopIndex());
  }

}// end class "TextSelection"
