/*
 * Copyright (C) 2013-2021 by XDEV Software, All Rights Reserved.
 *
 * This file is part of the RapidClipse Application Platform (RAP).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
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
