
package com.rapidclipse.framework.server.charts.calendar;

import java.io.Serializable;

import com.rapidclipse.framework.server.util.JavaScriptable;


/**
 * @author XDEV Software
 *
 */
public interface CellColor extends Serializable, JavaScriptable
{
	public String stroke();

	public Double strokeWidth();

	public Double strokeOpacity();

	public static Default New(final String stroke)
	{
		return new Default(stroke, null, null);
	}

	public static Default New(final String stroke, final Double strokeWidth)
	{
		return new Default(stroke, strokeWidth, null);
	}

	public static Default New(final String stroke, final Double strokeWidth, final Double strokeOpacity)
	{
		return new Default(stroke, strokeWidth, strokeOpacity);
	}

	public static class Default implements CellColor
	{
		private final String stroke;
		private final Double strokeWidth;
		private final Double strokeOpacity;

		Default(final String stroke, final Double strokeWidth, final Double strokeOpacity)
		{
			super();

			this.stroke        = stroke;
			this.strokeWidth   = strokeWidth;
			this.strokeOpacity = strokeOpacity;
		}

		@Override
		public String stroke()
		{
			return this.stroke;
		}

		@Override
		public Double strokeWidth()
		{
			return this.strokeWidth;
		}

		@Override
		public Double strokeOpacity()
		{
			return this.strokeOpacity;
		}

		@Override
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			obj.putIfNotNull("stroke", this.stroke);
			obj.putIfNotNull("strokeWidth", this.strokeWidth);
			obj.putIfNotNull("strokeOpacity", this.strokeOpacity);
			return obj.js();
		}

	}

}
