/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
 *
 * This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * which accompanies this distribution, and is available at
 * https://www.eclipse.org/legal/epl-2.0/
 *
 * SPDX-License-Identifier: EPL-2.0
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
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
	
	public static FilterOperatorRegistry New()
	{
		return new Implementation();
	}
	
	public static FilterOperatorRegistry Default()
	{
		final FilterOperatorRegistry registry = New();
		
		ServiceLoader.forType(FilterOperator.class).services().forEach(registry::put);
		
		return registry;
	}
	
	public static class Implementation implements FilterOperatorRegistry
	{
		private final Map<String, FilterOperator> registry = new LinkedHashMap<>();
		
		public Implementation()
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
