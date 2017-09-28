package com.appliedminds.martinix.ui;

import java.util.List;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.StringTokenizer;

import com.appliedminds.martini.DrawableGraphElement;
import com.appliedminds.martini.MartiniError;

/**
 * A TypeInfo contains the type information of a
 * DrawableGraphElement. A graph element may have multiple types and
 * each type is associated with an visibility flag.
 *
 * <p>The DrawableGraphElement may have the following type properties.
 *
 * <p><pre>
 *   graph [
 *     ...
 *     node [
 *       ...
 *       data [
 *         types               "(type1, type2)"
 *         typeVisibilities    "(true, false)"
 *         ...
 *       ]
 *       ...
 *     ]
 *     ...
 *   ]
 * </pre>
 *
 * <p>In the above example, the first type, type1, is visible and the
 * second type, type2, is not visible.
 *
 * @author daepark@apmindsf.com
 */
public class TypeInfo {

  /** type property */
  public static final String P_TYPES = "types";


  /** type visibility property */
  public static final String P_TYPE_VISIBILITIES = "typeVisibilities";


  /** String equivalent to null */
  protected static final String NULL  = "null";


  /** String equivalent to the boolean value */
  protected static final String TRUE  = "true";


  /** String equivalent to the boolean value */
  protected static final String FALSE = "false";

  private DrawableGraphElement _elt;
  private String[] _types;
  private String[] _typeVisibilities;


  /**
   * Factory method to obtain a TypeInfo for a DrawableGraphElement.
   *
   * @param elt The DrawableGraphElement to obtain the type
   * information for.
   */
  public static TypeInfo getTypeInfo(DrawableGraphElement elt) {
    //
    // Fix: TypeInfo should be cached instead of creating a new one.
    //
    return (new TypeInfo(elt));
  }


  /**
   * private constructor.
   *
   * @see #getTypeInfo
   */
  protected TypeInfo(DrawableGraphElement elt) {
    init(elt);
  }


  /**
   * private constructor.
   *
   * @see #getTypeInfo
   */
  private TypeInfo() { }


  /**
   * Collect all types for the given DrawableGraphElement.
   */
  protected void init(DrawableGraphElement elt) {
    _elt = elt;

    String types = _elt.getProperty(P_TYPES);
    String typeVisibilities = _elt.getProperty(P_TYPE_VISIBILITIES);

    if (types == null || typeVisibilities == null) {
      types = "()";
      typeVisibilities = "()";
    }

    _types = StringUtils.parseStringList(types);
    _typeVisibilities = StringUtils.parseStringList(typeVisibilities);


    if (_types.length != _typeVisibilities.length)
    {
      // if we have a valid gml this should not happen
      throw(new MartiniError("A DrawableGraphElement must have the same number of \"types\" and \"typeVisibilities\" values"));
    }
  }


  /**
   * Get the total number of ("ON" and "OFF") types.
   */
  public int getTypeCount() {
    return (_types.length);
  }


  /**
   * Get the total number of types that are visible.
   *
   * @return the total number of types that are visible.
   */
  public int getVisibleTypeCount() {
    int count = 0;
    for (int i = 0; i < _typeVisibilities.length; i++) {
      if (TRUE.equals(_typeVisibilities[i])) {
        count++;
      }
    }
    return (count);
  }


  /**
   * Get the type at the specified index.
   *
   * @param index the index of the type where 0 &lt;= index &lt;
   * getTypeCount();
   * @return the type at the specified index.
   */
  public String getType(int index) {
    return (_types[index]);
  }


  /**
   * Set the type at the specified index.
   *
   * @param index the index of the type where 0 &lt;= index &lt;
   * getTypeCount();
   * @param type the new type.
   */
  public void setType(int index, String type) {
    if (!getType(index).equals(type)) {
      _types[index] = type;

      // modify types property
      String val = StringUtils.toStringList(_types);
      _elt.setProperty(P_TYPES, val);
    }
  }


  /**
   * Set the visiblity of the type at the specified index.
   *
   * @param index the index of the type where 0 &lt;= index &lt;
   * getTypeCount();
   * @param visible true set to visible, false set to invisible.
   */
  public void setTypeVisibility(int index, boolean visible) {
    if (isTypeVisible(index) != visible) {
      _typeVisibilities[index] = (visible ? TRUE : FALSE);

      // modify visibilities property
      String val = StringUtils.toStringList(_typeVisibilities);
      _elt.setProperty(P_TYPE_VISIBILITIES, val);
    }
  }


  /**
   * Is the type at the specified index set to visible.
   *
   * @param index the index of the type where 0 &lt;= index &lt;
   * getTypeCount();
   * @return TRUE if visible, FALSE if invisible.
   */
  public boolean isTypeVisible(int index) {
    return (TRUE.equals(_typeVisibilities[index]));
  }


  /**
   * Add a new type to this TypeInfo.
   *
   * @param type the new type
   * @param visible the visiblity of the new type.
   */
  public void add(String type, boolean visible) {
    check();

    String[] newTypes = new String[_types.length + 1];
    String[] newTypeVisibilities = new String[_typeVisibilities.length + 1];

    System.arraycopy(_types, 0, newTypes, 0, _types.length);
    System.arraycopy(_typeVisibilities, 0, newTypeVisibilities, 0, _typeVisibilities.length);

    newTypes[newTypes.length - 1] = type;
    newTypeVisibilities[newTypeVisibilities.length - 1] = visible ? TRUE : FALSE;

    _types = newTypes;
    _typeVisibilities = newTypeVisibilities;

    // modify types property
    String val = StringUtils.toStringList(_types);
    _elt.setProperty(P_TYPES, val);

    // modify visibilities property
    val = StringUtils.toStringList(_typeVisibilities);
    _elt.setProperty(P_TYPE_VISIBILITIES, val);
  }


  /**
   * Get the graph element associated with this type info.
   */
  protected DrawableGraphElement getElement() {
    return (_elt);
  }


  /**
   * Check that this TypeInfo is in consistent state.
   *
   * @throws IllegalStateException if type and visibility count are not equal.
   */
  protected void check() {
    if (getTypeCount() != _typeVisibilities.length) {
      throw (new IllegalStateException("TypeInfo type and visibility count are not equal"));
    }
  }

} // end class TypeInfo
