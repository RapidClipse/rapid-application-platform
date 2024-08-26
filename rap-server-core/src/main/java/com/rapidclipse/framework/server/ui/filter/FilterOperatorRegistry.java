/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.ui.filter;

import java.io.Serializable;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;

import com.rapidclipse.framework.server.util.ServiceLoader;


/**
 * @author XDEV Software
 *
 */
public interface FilterOperatorRegistry extends Serializable
{
	public FilterOperatorRegistry put(FilterOperator operator);
	
	public default FilterOperatorRegistry putAll(final FilterOperator... operators)
	{
		for(final FilterOperator operator : operators)
		{
			put(operator);
		}
		
		return this;
	}
	
	public default FilterOperatorRegistry putAll(final Iterable<? extends FilterOperator> operators)
	{
		for(final FilterOperator operator : operators)
		{
			put(operator);
		}
		
		return this;
	}
	
	public FilterOperator remove(String key);
	
	public FilterOperator get(String key);
	
	public Collection<FilterOperator> getAll();
	
	public static FilterOperatorRegistry Empty()
	{
		return new Default();
	}
	
	public static FilterOperatorRegistry Default()
	{
		final FilterOperatorRegistry registry = Empty();
		
		ServiceLoader.forType(FilterOperator.class).services().forEach(registry::put);
		
		return registry;
	}
	
	public static class Default implements FilterOperatorRegistry
	{
		private final Map<String, FilterOperator> registry = new LinkedHashMap<>();
		
		protected Default()
		{
			super();
		}
		
		@Override
		public FilterOperatorRegistry put(final FilterOperator operator)
		{
			this.registry.put(operator.key(), operator);
			
			return this;
		}
		
		@Override
		public FilterOperator remove(final String key)
		{
			return this.registry.remove(key);
		}
		
		@Override
		public FilterOperator get(final String key)
		{
			return this.registry.get(key);
		}
		
		@Override
		public Collection<FilterOperator> getAll()
		{
			return this.registry.values();
		}
	}
}
