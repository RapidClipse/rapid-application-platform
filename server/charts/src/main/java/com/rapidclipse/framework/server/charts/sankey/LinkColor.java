
package com.rapidclipse.framework.server.charts.sankey;

import java.io.Serializable;

import com.rapidclipse.framework.server.util.JavaScriptable;


/**
 * @author XDEV Software
 *
 */
public interface LinkColor extends Serializable, JavaScriptable
{
	public String stroke();
	
	public Double strokeWidth();
	
	public String fill();
	
	public Double fillOpacity();
	
	public static LinkColor
		New(final String stroke, final Double strokeWidth, final String fill, final Double fillOpacity)
	{
		return new Default(stroke, strokeWidth, fill, fillOpacity);
	}
	
	public static class Default implements LinkColor
	{
		private final String stroke;
		private final Double strokeWidth;
		private final String fill;
		private final Double fillOpacity;
		
		Default(final String stroke, final Double strokeWidth, final String fill, final Double fillOpacity)
		{
			super();
			
			this.stroke      = stroke;
			this.strokeWidth = strokeWidth;
			this.fill        = fill;
			this.fillOpacity = fillOpacity;
		}
		
		@Override
		public String stroke()
		{
			return this.stroke;
		}
		
		@Override
		public Double strokeWidth()
		{
			return this.strokeWidth;
		}
		
		@Override
		public String fill()
		{
			return this.fill;
		}
		
		@Override
		public Double fillOpacity()
		{
			return this.fillOpacity;
		}
		
		@Override
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			obj.putIfNotNull("stroke", this.stroke);
			obj.putIfNotNull("strokeWidth", this.strokeWidth);
			obj.putIfNotNull("fill", this.fill);
			obj.putIfNotNull("fillOpacity", this.fillOpacity);
			return obj.js();
		}
		
	}
	
}
