package com.appliedminds.martini.animation;

/**
 * <b>Transform</b> represents the 3 row by 3 column matrix with an
 * implied last row of <code>[0 0 1]</code> used by an AffineTransform
 * to transforme source coordinates <code>(x, y)</code> into
 * destination coordinates <code>(x', y')</code> by considering them
 * to be a column vector and multiplying the coordinate vector by the
 * matrix according to the following process:
 * 
 * <p>
 * <code>
 * <pre>
 * [ x']   [  m00  m01  m02  ] [ x ]   [ m00x + m01y + m02 ]
 * [ y'] = [  m10  m11  m12  ] [ y ] = [ m10x + m11y + m12 ]
 * [ 1 ]   [   0    0    1   ] [ 1 ]   [         1         ]
 * </pre>
 * </code>
 *
 * @see java.awt.geom.AffineTransform
 * @author daepark@apmindsf.com
 */
public class Transform {

	private double _m00;
	private double _m10;
	private double _m01;
	private double _m11;
	private double _m02;
	private double _m12;

	/**
	 * Sets this transform to the matrix specified by the 6 double
	 * precision values.
	 *
	 * @param m00,&nbsp;m01,&nbsp;m02,&nbsp;m10,&nbsp;m11,&nbsp;m12 the
	 * 6 values that compose the 3x3 transformation matrix
	 * @see java.awt.geom.AffineTransform#setTransform
	 */
	public Transform(double m00, 
									 double m10, 
									 double m01, 
									 double m11, 
									 double m02, 
									 double m12)
	{
		this.setTransform(m00, m10, m01, m11, m02, m12);
	}

	/**
	 * Sets this transform to the matrix specified by the 6 double
	 * precision values.
	 *
	 * @param m00,&nbsp;m01,&nbsp;m02,&nbsp;m10,&nbsp;m11,&nbsp;m12 the
	 * 6 values that compose the 3x3 transformation matrix
	 * @see java.awt.geom.AffineTransform#setTransform
	 */
	public void setTransform(double m00, 
													 double m10, 
													 double m01, 
													 double m11, 
													 double m02, 
													 double m12) 
	{
		_m00 = m00;
		_m10 = m10;
		_m01 = m01;
		_m11 = m11;
		_m02 = m02;
		_m12 = m12;
	}

	/**
	 * Retrieves the 6 specifiable values of the affine transformation,
	 * and places them into an array of double precisions values.  The
	 * values are stored in the array as
	 * {&nbsp;m00&nbsp;m10&nbsp;m01&nbsp;m11&nbsp;m02&nbsp;m12&nbsp;}.
	 * An array of 4 doubles can also be specified, in which case only
	 * the first four elements representing the non-transform parts of
	 * the array are retrieved and the values are stored into the array
	 * as {&nbsp;m00&nbsp;m10&nbsp;m01&nbsp;m11&nbsp;}
	 *
	 * @param matrix the double array used to store the returned values.
	 * @see java.awt.geom.AffineTransform#getMatrix
	 */
	public void getMatrix(double[] flatmatrix) {
		flatmatrix[0] = _m00;
		flatmatrix[1] = _m10;
		flatmatrix[2] = _m01;
		flatmatrix[3] = _m11;
		if (flatmatrix.length > 5) {
			flatmatrix[4] = _m02;
			flatmatrix[5] = _m12;
		}
	}

} // end class TransformTarget.Transform
