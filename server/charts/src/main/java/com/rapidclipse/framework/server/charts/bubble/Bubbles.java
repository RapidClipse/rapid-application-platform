
package com.rapidclipse.framework.server.charts.bubble;

import java.io.Serializable;

import com.rapidclipse.framework.server.charts.TextStyle;
import com.rapidclipse.framework.server.util.JavaScriptable;


/**
 * @author XDEV Software
 *
 */
public interface Bubbles extends Serializable, JavaScriptable
{
	public Double opacity();
	
	public String stroke();
	
	public TextStyle textStyle();
	
	public static Bubbles New(final String stroke)
	{
		return new Default(null, stroke, null);
	}
	
	public static Bubbles New(final Double opacity, final String stroke)
	{
		return new Default(opacity, stroke, null);
	}
	
	public static Bubbles New(final String stroke, final TextStyle textStyle)
	{
		return new Default(null, stroke, textStyle);
	}
	
	public static Bubbles New(final Double opacity, final TextStyle textStyle)
	{
		return new Default(opacity, null, textStyle);
	}
	
	public static Bubbles New(final Double opacity, final String stroke, final TextStyle textStyle)
	{
		return new Default(opacity, stroke, textStyle);
	}
	
	public static class Default implements Bubbles
	{
		private final Double    opacity;
		private final String    stroke;
		private final TextStyle textStyle;
		
		Default(final Double opacity, final String stroke, final TextStyle textStyle)
		{
			super();
			
			this.opacity   = opacity;
			this.stroke    = stroke;
			this.textStyle = textStyle;
		}
		
		@Override
		public Double opacity()
		{
			return this.opacity;
		}
		
		@Override
		public String stroke()
		{
			return this.stroke;
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
			obj.putIfNotNull("opacity", this.opacity);
			obj.putIfNotNull("stroke", this.opacity);
			obj.putIfNotNull("textStyle", this.textStyle);
			return obj.js();
		}

	}

}
