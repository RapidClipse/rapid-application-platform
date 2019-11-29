
package com.rapidclipse.framework.server.charts;

import java.io.Serializable;

import com.rapidclipse.framework.server.util.JavaScriptable;


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface GridLines extends Serializable, JavaScriptable
{
	public String color();
	
	public Integer count();
	
	public static GridLines New(final String color)
	{
		return new Default(color, null);
	}
	
	public static GridLines New(final Integer count)
	{
		return new Default(null, count);
	}
	
	public static GridLines New(final String color, final Integer count)
	{
		return new Default(color, count);
	}
	
	public static class Default implements GridLines
	{
		private final String  color;
		private final Integer count;
		
		Default(final String color, final Integer count)
		{
			super();
			
			this.color = color;
			this.count = count;
		}
		
		@Override
		public String color()
		{
			return this.color;
		}
		
		@Override
		public Integer count()
		{
			return this.count;
		}
		
		@Override
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			obj.putIfNotNull("color", this.color);
			obj.putIfNotNull("count", this.count);
			return obj.js();
		}
		
	}
	
}
