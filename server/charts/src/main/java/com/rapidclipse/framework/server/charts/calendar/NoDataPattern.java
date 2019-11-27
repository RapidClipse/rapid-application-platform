
package com.rapidclipse.framework.server.charts.calendar;

import java.io.Serializable;

import com.rapidclipse.framework.server.util.JavaScriptable;


/**
 * @author XDEV Software
 *
 */
public interface NoDataPattern extends Serializable, JavaScriptable
{
	public String backgroundColor();

	public String color();

	public static NoDataPattern New(final String backgroundColor, final String color)
	{
		return new Default(backgroundColor, color);
	}

	public static class Default implements NoDataPattern
	{
		private final String backgroundColor;
		private final String color;

		Default(final String backgroundColor, final String color)
		{
			super();

			this.backgroundColor = backgroundColor;
			this.color           = color;
		}

		@Override
		public String backgroundColor()
		{
			return this.backgroundColor;
		}

		@Override
		public String color()
		{
			return this.color;
		}

		@Override
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			obj.putIfNotNull("backgroundColor", this.backgroundColor);
			obj.putIfNotNull("color", this.color);
			return obj.js();
		}

	}

}
