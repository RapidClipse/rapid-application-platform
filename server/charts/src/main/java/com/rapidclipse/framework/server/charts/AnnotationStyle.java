
package com.rapidclipse.framework.server.charts;

import java.io.Serializable;

import com.rapidclipse.framework.server.util.JavaScriptable;


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface AnnotationStyle extends Serializable, JavaScriptable
{
	public Stem stem();
	
	public String style();
	
	public static AnnotationStyle New(final Stem stem)
	{
		return new Default(stem, null);
	}
	
	public static AnnotationStyle New(final String style)
	{
		return new Default(null, style);
	}
	
	public static AnnotationStyle New(final Stem stem, final String style)
	{
		return new Default(stem, style);
	}
	
	public static class Default implements AnnotationStyle
	{
		private final Stem   stem;
		private final String style;
		
		Default(final Stem stem, final String style)
		{
			super();
			
			this.stem  = stem;
			this.style = style;
		}
		
		@Override
		public Stem stem()
		{
			return this.stem;
		}
		
		@Override
		public String style()
		{
			return this.style;
		}
		
		@Override
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			obj.putIfNotNull("stem", this.stem);
			obj.putIfNotNull("style", this.style);
			return obj.js();
		}
	}
}
