package com.appliedminds.martini.animation;

import java.awt.Graphics2D;
import java.awt.Paint;
import java.awt.Stroke;
import java.awt.font.TextLayout;
import java.awt.geom.AffineTransform;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;


/**
 * <b>Drawable</b> objects know how to render itself onto a
 * <code>java.awt.Graphics2D</code> object. A Drawable may have a
 * stacking order, thus implements a Comparable interface. That is, a
 * Drawable may want to be drawn first before another Drawable.
 *
 * @author daepark@apmindsf.com
 */
public interface Drawable extends Comparable {
	
	/**
	 * Render onto a <code>java.awt.Graphics2D</code> using the
	 * Drawable.Props properties.
	 *
	 * @param g the graphics this is rendering onto
	 * @param prop a Drawable.Props properties that contains properties
	 * for drawing the Drawable, such as paint colors, stroke width,
	 * transforms, etc.
	 */
	public void draw(Graphics2D g, Props props);


	/**
	 * Get the smallest rectangle enclosing the Drawable object.
	 */
	public Rectangle2D getBounds();


	/**
	 * <b>Drawable.Default</b> is the default comparable Drawable.
	 *
	 * @author daepark@apmindsf.com
	 */
	public abstract class Default implements Drawable {

		int __index;

		/**
		 * The index determines the sort order.
		 */
		public Default(int index) {
			__index = index;
		}

		/////////////////////////////
		// start Comparable interface

		public int compareTo(Object o) {
			return (__index - ((Default)o).__index);
		}

		// end Comparable interface
		/////////////////////////////

	} // end class Drawable.Default


	/**
	 * <b>Drawable.Shape</b> is a Drawable implementation for a
	 * <code>java.awt.Shape</code> object.
	 *
	 * @author daepark@apmindsf.com
	 */
	public class Shape extends Default {

		private java.awt.Shape __shape;

		/**
		 * The index determines the sort order of this Drawable.
		 *
		 * @param index the sort order index
		 * @param shape the java.awt.Shape that this can draw
		 */
		public Shape(int index, java.awt.Shape shape) {
			super(index);
			setShape(shape);
		}

		/**
		 * Set the java.awt.Shape that this can draw.
		 */
		public void setShape(java.awt.Shape shape) {
			__shape = shape;
		}

		/**
		 * Get the java.awt.Shape that this can draw.
		 */
		public java.awt.Shape getShape() {
			return (__shape);
		}

		///////////////////////////
		// start Drawable interface
		
		public Rectangle2D getBounds() {
			return (__shape.getBounds2D());
		}

		public void draw(Graphics2D g, Props props) {
			Stroke stroke = props.getStroke();
			Paint penPaint = props.getPenPaint();
			Paint fillPaint = props.getFillPaint();
			Transform transform = props.getTransform();
			AffineTransform oldTransform = g.getTransform();

			// set transform if any
			if (transform != null) {
				double[] matrix = new double[6];
				transform.getMatrix(matrix);
				g.setTransform(new AffineTransform(matrix));
			}

			if (fillPaint != null) {
				g.setPaint(fillPaint);
				g.fill(__shape);
			}

			if (penPaint != null) {
				if (stroke != null) {
					g.setStroke(stroke);
				}

				g.setPaint(penPaint);
				g.draw(__shape);
			}

			// reset transform back to old transform
			if (transform != null) {
				g.setTransform(oldTransform);
			}
		}

		// end Drawable interface
		///////////////////////////

	} // end class Drawable.Shape


	/**
	 * <b>Drawable.Text</b> is a Drawable implementation for a
	 * <code>java.awt.font.TextLayout</code> object.
	 *
	 * @author daepark@apmindsf.com
	 */
	public class Text extends Default {

		private TextLayout __layout;
		private Point2D      __center;

		/**
		 * The index determines the sort order of this Drawable.
		 * 
		 * @param index the index determines the sort order
		 * @param layout the java.awt.font.TextLayout that this can draw.
		 */
		public Text(int index, TextLayout layout) {
			super(index);
			__layout = layout;
			__center = new Point2D.Double(layout.getBounds().getCenterX(),
																		layout.getBounds().getCenterY());
		}
		
		/**
		 * The index determines the sort order of this Drawable.
		 * 
		 * @param index the index determines the sort order
		 * @param layout the java.awt.font.TextLayout that this can draw.
		 * @param center the centered position of the text when drawn.
		 */
		public Text(int index, TextLayout layout, Point2D center) {
			super(index);
			__layout = layout;
			__center = center;
			 
		}

		/**
		 * Set the java.awt.font.TextLayout that this can draw.
		 */
		public void setTextLayout(TextLayout layout, 
															Point2D center) 
		{
			__layout = layout;
			__center = center;
		}

		/**
		 * Set the centered position of the text when drawn.
		 */
		public void setCenter(Point2D center) {
			__center = center;
		}

		/**
		 * Get the java.awt.font.TextLayout that this can draw.
		 */
		public TextLayout getTextLayout() {
			return (__layout);
		}

		/**
		 * Get the centered position of the text when drawn.
		 */
		public Point2D getCenter() {
			return(__center);
		}

		///////////////////////////
		// start Drawable interface

		public Rectangle2D getBounds() {
			return (__layout.getBounds());
		}

		public void draw(Graphics2D g, Props props) {
			Paint fontPaint = props.getFontPaint();
			Transform transform = props.getTransform();
			AffineTransform oldTransform = g.getTransform();

			Point2D textPosition = new Point2D.Double
				(__center.getX() - __layout.getBounds().getWidth() / 2,
				 __center.getY() + __layout.getAscent() / 2);

			// set transform if any
			if (transform != null) {
				double[] matrix = new double[6];
				transform.getMatrix(matrix);
				AffineTransform af = new AffineTransform(matrix);
				g.setTransform(af);
				//				textPosition = af.transform(textPosition, null);
			}

			if (fontPaint != null) {
				g.setPaint(fontPaint);
				__layout.draw(g, 
											(float)(textPosition.getX()),
											(float)(textPosition.getY()));
			}

			// reset transform back to old transform
			if (transform != null) {
				g.setTransform(oldTransform);
			}
		}

		// end Drawable interface
		///////////////////////////

	} // end class Drawable.Text


	/**
	 * <b>Drawable.Props</b> contain drawing properties used by a
	 * Drawable when rendering itself onto a Graphics2D object.
	 */
	public class Props {

		private Stroke __stroke = null;
		private Paint  __penPaint = null;
		private Paint  __fillPaint = null;
		private Paint  __fontPaint = null;
		private Transform  __transform = null;
		
		public Stroke getStroke() { return (__stroke); }
		public void   setStroke(Stroke stroke) { __stroke = stroke; }
		
		public Paint getPenPaint() { return (__penPaint); }
		public void  setPenPaint(Paint penPaint) { __penPaint = penPaint; }
		
		public Paint getFillPaint() { return (__fillPaint); }
		public void  setFillPaint(Paint fillPaint) { __fillPaint = fillPaint; }

		public Paint getFontPaint() { return (__fontPaint); }
		public void  setFontPaint(Paint fontPaint) { __fontPaint = fontPaint; }

		public Transform getTransform() { return (__transform); }
		public void setTransform(Transform transform) { __transform = transform; }

		public String debug() {
			return ("[drawprops: (stroke:     " + getStroke() + ")" +
							" (pen paint :  " + getPenPaint() + ")" +
							" (fill paint: " + getFillPaint() + ")" +
							" (font paint: " + getFontPaint() + ")" +
							" (transform : " + getTransform() + ")");
			
		}

	}  // end class Props

} // end Drawable interface
