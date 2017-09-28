package com.appliedminds.martini;


/**
 * A simple record of a property change. This object keeps track of a
 * property's old value and the new value.
 *
 * @author daepark@apmindsf.com
 */
public class PropertyChange {

	private String _property;
	private String _oldValue;
	private String _newValue;


	/**
	 * Initialize a property change record.
	 *
	 * @param property the changed property
	 * @param oldValue the old property value
	 * @param newValue the new property value
	 */
	public PropertyChange(String property, String oldValue, String newValue) {
		_property = property;
		_oldValue = oldValue;
		_newValue = newValue;
	}


	/**
	 * @return the property that was changed.
	 */
	public String getProperty() {
		return (_property);
	}


	/**
	 * @return the old property value.
	 */
	public String getOldValue() {
		return (_oldValue);
	}


	/**
	 * @return the new property value.
	 */
	public String getNewValue() {
		return (_newValue);
	}

} // end class PropertyChange
