/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
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
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import com.vaadin.flow.component.HasElement;
import com.vaadin.flow.component.HasSize;


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface Chart extends HasElement, HasSize
{
	public ChartModel getModel();
	
	public Properties properties();
	
	public static class Properties implements Serializable, JavaScriptable
	{
		private final Map<String, Object>              properties        = new HashMap<>();
		private final Map<String, Map<Object, Object>> indexedProperties = new HashMap<>();
		
		protected Properties()
		{
			super();
		}
		
		public <T> void put(final String name, final T value)
		{
			if(value == null)
			{
				this.properties.remove(name);
			}
			else
			{
				this.properties.put(name, value);
			}
		}
		
		@SuppressWarnings("unchecked")
		public <T> T get(final String name)
		{
			return (T)this.properties.get(name);
		}
		
		public <T> void putIndexed(final String name, final Object index, final T value)
		{
			this.indexedProperties.computeIfAbsent(name, n -> new HashMap<>()).put(index, value);
		}
		
		@SuppressWarnings("unchecked")
		public <T> T getIndexed(final String name, final Object index)
		{
			final Map<Object, Object> map = this.indexedProperties.get(name);
			return map != null
				? (T)map.get(index)
				: null;
		}
		
		@SuppressWarnings("unchecked")
		public <T> T removeIndexed(final String name, final Object index)
		{
			T                         removed = null;
			final Map<Object, Object> map     = this.indexedProperties.get(name);
			if(map != null)
			{
				removed = (T)map.remove(index);
				if(map.isEmpty())
				{
					this.indexedProperties.remove(name);
				}
			}
			return removed;
		}
		
		public void removeAllIndexed(final String name)
		{
			this.indexedProperties.remove(name);
		}
		
		@Override
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			putAll(obj, this.properties);
			
			this.indexedProperties.entrySet().forEach(ie -> {
				final String              key    = ie.getKey();
				final Map<Object, Object> values = ie.getValue();
				if(values != null && values.size() > 0)
				{
					final ObjectHelper valuesObj = new ObjectHelper();
					putAll(valuesObj, values);
					obj.put(key, valuesObj);
				}
			});
			
			return obj.js();
		}

		private void putAll(final ObjectHelper obj, final Map<?, ?> map)
		{
			map.entrySet().forEach(e -> {
				final Object key   = e.getKey();
				final Object value = e.getValue();
				if(value instanceof Collection)
				{
					obj.putIfNotNull(key.toString(), new ArrayHelper().addAll((Collection<?>)value));
				}
				else
				{
					obj.putIfNotNull(key.toString(), value);
				}
			});
		}
	}
}
