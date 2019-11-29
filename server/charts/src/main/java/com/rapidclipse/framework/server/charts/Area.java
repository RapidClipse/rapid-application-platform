
package com.rapidclipse.framework.server.charts;

import java.io.Serializable;

import com.rapidclipse.framework.server.util.JavaScriptable;


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface Area extends Serializable, JavaScriptable
{
	public String left();
	
	public String top();
	
	public String width();
	
	public String height();
	
	public String backgroundColor();
	
	public static Area
		New(final String left, final String top, final String width, final String height)
	{
		return new Default(left, top, width, height, null);
	}
	
	public static Area
		New(final String backgroundColor)
	{
		return new Default(null, null, null, null, backgroundColor);
	}
	
	public static Area
		New(final String left, final String top, final String width, final String height, final String backgroundColor)
	{
		return new Default(left, top, width, height, backgroundColor);
	}
	
	public static class Default implements Area
	{
		private final String left;
		private final String top;
		private final String width;
		private final String height;
		private final String backgroundColor;
		
		Default(
			final String left,
			final String top,
			final String width,
			final String height,
			final String backgroundColor)
		{
			super();
			
			this.left            = left;
			this.top             = top;
			this.width           = width;
			this.height          = height;
			this.backgroundColor = backgroundColor;
		}
		
		@Override
		public String left()
		{
			return this.left;
		}
		
		@Override
		public String top()
		{
			return this.top;
		}
		
		@Override
		public String width()
		{
			return this.width;
		}
		
		@Override
		public String height()
		{
			return this.height;
		}
		
		@Override
		public String backgroundColor()
		{
			return this.backgroundColor;
		}
		
		@Override
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			obj.putIfNotNull("left", this.left);
			obj.putIfNotNull("top", this.top);
			obj.putIfNotNull("width", this.width);
			obj.putIfNotNull("height", this.height);
			obj.putIfNotNull("backgroundColor", this.backgroundColor);
			return obj.js();
		}
		
	}
	
}
