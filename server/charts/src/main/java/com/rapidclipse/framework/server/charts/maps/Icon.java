
package com.rapidclipse.framework.server.charts.maps;

import java.io.Serializable;

import com.rapidclipse.framework.server.util.JavaScriptable;


/**
 * @author XDEV Software
 *
 */
public interface Icon extends Serializable, JavaScriptable
{
	public String normal();
	
	public String selected();
	
	public static Icon New(final String normal, final String selected)
	{
		return new Default(normal, selected);
	}

	public static class Default implements Icon
	{
		private final String normal;
		private final String selected;
		
		Default(final String normal, final String selected)
		{
			super();
			
			this.normal   = normal;
			this.selected = selected;
		}
		
		@Override
		public String normal()
		{
			return this.normal;
		}
		
		@Override
		public String selected()
		{
			return this.selected;
		}

		@Override
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			obj.putIfNotNull("normal", this.normal);
			obj.putIfNotNull("selected", this.selected);
			return obj.js();
		}
	}
}
