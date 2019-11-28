
package com.rapidclipse.framework.server.charts.maps;

import java.io.Serializable;

import com.rapidclipse.framework.server.charts.TextStyle;
import com.rapidclipse.framework.server.util.JavaScriptable;


/**
 * @author XDEV Software
 *
 */
public interface Legend extends Serializable, JavaScriptable
{
	public String numberFormat();
	
	public TextStyle textStyle();
	
	public static Legend New(final String numberFormat, final TextStyle textStyle)
	{
		return new Default(numberFormat, textStyle);
	}
	
	public static Legend New(final String numberFormat)
	{
		return new Default(numberFormat, null);
	}
	
	public static Legend New(final TextStyle textStyle)
	{
		return new Default(null, textStyle);
	}

	public static class Default implements Legend
	{
		private final String    numberFormat;
		private final TextStyle textStyle;
		
		Default(final String numberFormat, final TextStyle textStyle)
		{
			super();
			
			this.numberFormat = numberFormat;
			this.textStyle    = textStyle;
		}

		@Override
		public String numberFormat()
		{
			return this.numberFormat;
		}
		
		@Override
		public TextStyle textStyle()
		{
			return this.textStyle;
		}

		@Override
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			obj.putIfNotNull("numberFormat", this.numberFormat);
			obj.putIfNotNull("textStyle", this.textStyle);
			return obj.js();
		}
		
	}
	
}
