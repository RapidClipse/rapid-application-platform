
package com.rapidclipse.framework.server.charts;

import java.io.Serializable;
import java.util.Objects;

import com.rapidclipse.framework.server.util.JavaScriptable;

import elemental.json.Json;


/**
 * @author XDEV Software
 *
 */
public interface Background extends Serializable, JavaScriptable
{
	public static Color Color(final String color)
	{
		return new Color(color);
	}
	
	public static class Color implements Background
	{
		private final String color;
		
		Color(final String color)
		{
			super();
			
			this.color = Objects.requireNonNull(color);
		}
		
		public String color()
		{
			return this.color;
		}
		
		@Override
		public String js()
		{
			return Json.create(this.color).toJson();
		}

	}
	
	public static StrokeFill StrokeFill(final String stroke, final Double strokeWidth, final String fill)
	{
		return new StrokeFill(stroke, strokeWidth, fill);
	}
	
	public static class StrokeFill implements Background
	{
		private final String stroke;
		private final Double strokeWidth;
		private final String fill;
		
		StrokeFill(final String stroke, final Double strokeWidth, final String fill)
		{
			super();
			
			this.stroke      = stroke;
			this.strokeWidth = strokeWidth;
			this.fill        = fill;
		}
		
		public String stroke()
		{
			return this.stroke;
		}
		
		public Double strokeWidth()
		{
			return this.strokeWidth;
		}
		
		public String fill()
		{
			return this.fill;
		}
		
		@Override
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			obj.putIfNotNull("stroke", this.stroke);
			obj.putIfNotNull("strokeWidth", this.strokeWidth);
			obj.putIfNotNull("fill", this.fill);
			return obj.js();
		}

	}

}
