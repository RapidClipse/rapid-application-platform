
package com.rapidclipse.framework.server.charts;

import java.io.Serializable;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import com.rapidclipse.framework.server.util.JavaScriptable;


/**
 * @author XDEV Software
 *
 */
public interface Styles extends Serializable, JavaScriptable
{
	public Map<String, Object> styles();

	public static interface Builder
	{
		public Builder put(String name, Number value);

		public Builder put(String name, String value);

		public Styles build();
		
		public static class Default implements Builder
		{
			private final Map<String, Object> styles = new HashMap<>();

			Default()
			{
				super();
			}
			
			@Override
			public Builder put(final String name, final Number value)
			{
				this.styles.put(name, value);
				return this;
			}

			@Override
			public Builder put(final String name, final String value)
			{
				this.styles.put(name, value);
				return this;
			}

			@Override
			public Styles build()
			{
				return new Styles.Default(this.styles);
			}
			
		}

	}

	public static Styles New(final Map<String, Object> styles)
	{
		return new Default(styles);
	}
	
	public static class Default implements Styles
	{
		private final Map<String, Object> styles;
		
		Default(final Map<String, Object> styles)
		{
			super();
			
			this.styles = Collections.unmodifiableMap(styles);
		}
		
		@Override
		public Map<String, Object> styles()
		{
			return this.styles;
		}
		
		@Override
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			this.styles.entrySet().forEach(e -> obj.putIfNotNull(e.getKey(), e.getValue()));
			return obj.js();
		}

	}

}
