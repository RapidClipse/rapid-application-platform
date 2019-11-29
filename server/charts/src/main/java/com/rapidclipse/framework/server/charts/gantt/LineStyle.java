
package com.rapidclipse.framework.server.charts.gantt;

import java.io.Serializable;

import com.rapidclipse.framework.server.util.JavaScriptable;


/**
 * @author XDEV Software
 *
 */
public interface LineStyle extends Serializable, JavaScriptable
{
	public String stroke();

	public Number strokeWidth();

	public static LineStyle New(final String stroke, final Number strokeWidth)
	{
		return new Default(stroke, strokeWidth);
	}

	public static class Default implements LineStyle
	{
		private final String stroke;
		private final Number strokeWidth;

		Default(final String stroke, final Number strokeWidth)
		{
			super();

			this.stroke      = stroke;
			this.strokeWidth = strokeWidth;
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
			obj.putIfNotNull("stroke", this.stroke);
			obj.putIfNotNull("strokeWidth", this.strokeWidth);
			return obj.js();
		}

	}

}
