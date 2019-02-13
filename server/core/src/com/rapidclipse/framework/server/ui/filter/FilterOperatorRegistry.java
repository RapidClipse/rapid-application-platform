/*
 * Copyright (C) 2013-2018 by XDEV Software, All Rights Reserved.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 *
 * For further information see
 * <http://www.rapidclipse.com/en/legal/license/license.html>.
 */

package com.rapidclipse.framework.server.ui.filter;


import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;

import com.rapidclipse.framework.server.util.ServiceLoader;


/**
 * @author XDEV Software
 *
 */
public interface FilterOperatorRegistry
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
			this.registry.put(operator.key(),operator);
			
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
