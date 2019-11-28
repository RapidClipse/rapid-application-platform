
package com.rapidclipse.framework.server.charts.gantt;

import java.io.Serializable;

import com.rapidclipse.framework.server.util.JavaScriptable;


/**
 * @author XDEV Software
 *
 */
public interface BarStyle extends Serializable, JavaScriptable
{
	public String fill();

	public static BarStyle New(final String fill)
	{
		return new Default(fill);
	}

	public static class Default implements BarStyle
	{
		private final String fill;

		Default(final String fill)
		{
			super();

			this.fill = fill;
		}

		@Override
		public String fill()
		{
			return this.fill;
		}

		@Override
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			obj.putIfNotNull("fill", this.fill);
			return obj.js();
		}

	}

}
