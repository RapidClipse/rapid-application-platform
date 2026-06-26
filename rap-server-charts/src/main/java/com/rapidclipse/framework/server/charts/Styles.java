/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.charts;

import java.io.Serializable;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;


/**
 * @author XDEV Software
 * @since 10.02.00
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
