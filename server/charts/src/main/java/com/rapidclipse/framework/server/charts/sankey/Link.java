
package com.rapidclipse.framework.server.charts.sankey;

import java.io.Serializable;
import java.util.List;

import com.rapidclipse.framework.server.util.JavaScriptable;


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface Link extends Serializable, JavaScriptable
{
	public LinkColor color();
	
	public List<String> colors();

	public ColorMode colorMode();
	
	public static Link New(final LinkColor color, final List<String> colors, final ColorMode colorMode)
	{
		return new Default(color, colors, colorMode);
	}
	
	public static class Default implements Link
	{
		private final LinkColor    color;
		private final List<String> colors;
		private final ColorMode    colorMode;
		
		Default(final LinkColor color, final List<String> colors, final ColorMode colorMode)
		{
			super();

			this.color     = color;
			this.colors    = colors;
			this.colorMode = colorMode;
		}

		@Override
		public LinkColor color()
		{
			return this.color;
		}
		
		@Override
		public List<String> colors()
		{
			return this.colors;
		}

		@Override
		public ColorMode colorMode()
		{
			return this.colorMode;
		}

		@Override
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			obj.putIfNotNull("color", this.color);
			obj.putIfNotNull("colors", new ArrayHelper().addAllStrings(this.colors));
			obj.putIfNotNull("colorMode", this.colorMode);
			return obj.js();
		}
	}
}
