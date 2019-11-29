
package com.rapidclipse.framework.server.charts;

import java.io.Serializable;

import com.rapidclipse.framework.server.util.JavaScriptable;


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface CandlestickColor extends Serializable, JavaScriptable
{
	public String fill();

	public String stroke();

	public Number strokeWidth();

	public static Default New(final String fill)
	{
		return new Default(fill, null, null);
	}

	public static Default New(final String fill, final String stroke)
	{
		return new Default(fill, stroke, null);
	}

	public static Default New(final String fill, final String stroke, final Number strokeWidth)
	{
		return new Default(fill, stroke, strokeWidth);
	}

	public static class Default implements CandlestickColor
	{
		private final String fill;
		private final String stroke;
		private final Number strokeWidth;

		Default(final String fill, final String stroke, final Number strokeWidth)
		{
			super();

			this.fill        = fill;
			this.stroke      = stroke;
			this.strokeWidth = strokeWidth;
		}

		@Override
		public String fill()
		{
			return this.fill;
		}

		@Override
		public String stroke()
		{
			return this.stroke;
		}

		@Override
		public Number strokeWidth()
		{
			return this.strokeWidth;
		}

		@Override
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			obj.putIfNotNull("fill", this.fill);
			obj.putIfNotNull("stroke", this.stroke);
			obj.putIfNotNull("strokeWidth", this.strokeWidth);
			return obj.js();
		}

	}

}
