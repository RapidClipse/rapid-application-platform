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
package com.rapidclipse.framework.server.data.provider;

import static java.util.Objects.requireNonNull;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import javax.persistence.Parameter;
import javax.persistence.TypedQuery;


/**
 * @author XDEV Software
 *
 */
public interface CriteriaParameterProvider extends Serializable
{
	public void setParameters(TypedQuery<?> query);
	
	public static CriteriaParameterProvider Empty()
	{
		return query -> {};
	}
	
	public static CriteriaParameterProvider SingleParameter(final String name, final Serializable value)
	{
		requireNonNull(name);
		
		return query -> query.setParameter(name, value);
	}
	
	public static <T, P extends Serializable> CriteriaParameterProvider
		SingleParameter(final Parameter<P> parameter, final P value)
	{
		requireNonNull(parameter);
		
		return query -> query.setParameter(parameter, value);
	}
	
	public static CriteriaParameterProvider NamedMap(final Map<String, Serializable> parameterMap)
	{
		requireNonNull(parameterMap);
		
		return query -> Static.setParametersNamedMap(query, parameterMap);
	}
	
	public static CriteriaParameterProvider ParameterMap(final Map<Parameter<?>, Serializable> parameterMap)
	{
		requireNonNull(parameterMap);
		
		return query -> Static.setParametersParameterMap(query, parameterMap);
	}
	
	public static Builder Builder()
	{
		return new Builder.Default();
	}
	
	public static interface Builder
	{
		public Builder add(String name, Serializable value);
		
		public <P extends Serializable> Builder add(Parameter<P> parameter, P value);
		
		public CriteriaParameterProvider build();
		
		public static class Default implements Builder
		{
			private final Map<String, Serializable>       nameToValue  = new HashMap<>();
			private final Map<Parameter<?>, Serializable> paramToValue = new HashMap<>();

			protected Default()
			{
				super();
			}
			
			@Override
			public Builder add(final String name, final Serializable value)
			{
				this.nameToValue.put(requireNonNull(name), value);
				
				return this;
			}
			
			@Override
			public <P extends Serializable> Builder add(final Parameter<P> parameter, final P value)
			{
				this.paramToValue.put(requireNonNull(parameter), value);
				
				return this;
			}
			
			@Override
			public CriteriaParameterProvider build()
			{
				return query -> {
					
					Static.setParametersNamedMap(query, this.nameToValue);
					Static.setParametersParameterMap(query, this.paramToValue);
				};
			}
		}
	}
	
	public static class Static
	{
		public static void
			setParametersNamedMap(final TypedQuery<?> query, final Map<String, Serializable> parameterMap)
		{
			for(final Entry<String, Serializable> entry : parameterMap.entrySet())
			{
				final String name  = entry.getKey();
				final Object value = entry.getValue();
				query.setParameter(name, value);
			}
		}
		
		@SuppressWarnings({"rawtypes", "unchecked"})
		public static void
			setParametersParameterMap(
				final TypedQuery<?> query,
				final Map<Parameter<?>, Serializable> parameterMap)
		{
			for(final Entry<Parameter<?>, Serializable> entry : parameterMap.entrySet())
			{
				final Parameter parameter = entry.getKey();
				final Object    value     = entry.getValue();
				query.setParameter(parameter, value);
			}
		}
		
		private Static()
		{
			throw new Error();
		}
	}
}
