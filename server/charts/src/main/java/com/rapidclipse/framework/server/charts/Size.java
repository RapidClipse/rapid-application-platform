
package com.rapidclipse.framework.server.charts;

import java.io.Serializable;

import com.rapidclipse.framework.server.util.JavaScriptable;

import elemental.json.Json;


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface Size extends Serializable, JavaScriptable
{
	public static Size Pixels(final int value)
	{
		return new Pixels(value);
	}
	
	public static Size Percent(final int value)
	{
		return new Percent(value);
	}
	
	public static class Pixels implements Size
	{
		private final Integer value;
		
		Pixels(final int value)
		{
			super();
			
			this.value = value;
		}
		
		@Override
		public String js()
		{
			return Json.create(this.value).toJson();
		}
	}
	
	public static class Percent implements Size
	{
		private final String value;
		
		Percent(final int value)
		{
			super();
			
			this.value = Integer.toString(value).concat("%");
		}
		
		@Override
		public String js()
		{
			return Json.create(this.value).toJson();
		}
	}
}
