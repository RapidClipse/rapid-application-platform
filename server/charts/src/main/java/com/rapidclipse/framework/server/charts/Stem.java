
package com.rapidclipse.framework.server.charts;

import java.io.Serializable;

import com.rapidclipse.framework.server.util.JavaScriptable;


/**
 * @author XDEV Software
 *
 */
public interface Stem extends Serializable, JavaScriptable
{
	public String color();

	public Number length();

	public static Stem New(final String color)
	{
		return new Default(color, null);
	}
	
	public static Stem New(final Number length)
	{
		return new Default(null, length);
	}
	
	public static Stem New(final String color, final Number length)
	{
		return new Default(color, length);
	}

	public static class Default implements Stem
	{
		private final String color;
		private final Number length;

		Default(final String stemColor, final Number stemLength)
		{
			super();

			this.color  = stemColor;
			this.length = stemLength;
		}

		@Override
		public String color()
		{
			return this.color;
		}

		@Override
		public Number length()
		{
			return this.length;
		}

		@Override
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			obj.putIfNotNull("color", this.color);
			obj.putIfNotNull("length", this.length);
			return obj.js();
		}
	}
}
