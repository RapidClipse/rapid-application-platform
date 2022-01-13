/*
 * Copyright (C) 2013-2022 by XDEV Software, All Rights Reserved.
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
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software - initial API and implementation
 */

package com.rapidclipse.framework.server.reports.grid.column;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;

import com.rapidclipse.framework.server.reports.grid.column.headerresolving.BeanKeyCaptionColumnHeaderResolvingStrategy;
import com.rapidclipse.framework.server.reports.grid.column.headerresolving.ColumnHeaderResolvingStrategy;
import com.rapidclipse.framework.server.reports.grid.column.headerresolving.ManualColumnHeaderResolvingStrategy;
import com.rapidclipse.framework.server.reports.grid.column.headerresolving.VaadinInternalRenderingColumnHeaderResolvingStrategy;
import com.vaadin.flow.component.grid.Grid.Column;
import com.vaadin.flow.function.SerializableFunction;


/**
 * Builds a function (from multiple strategies) that resolves the text for a column.<br/>
 * If no strategies are specified the fallback {@link Column#getKey()} is used.
 * 
 * @author XDEV Software
 *
 */
public class ColumnConfigurationHeaderResolvingStrategyBuilder
{
	private final List<ColumnHeaderResolvingStrategy> strategies = new ArrayList<>();
	
	/**
	 * Uses the {@link VaadinInternalRenderingColumnHeaderResolvingStrategy}
	 *
	 * @return
	 */
	public ColumnConfigurationHeaderResolvingStrategyBuilder withVaadinInternalHeaderStrategy()
	{
		return withStrategy(new VaadinInternalRenderingColumnHeaderResolvingStrategy());
	}
	
	/**
	 * Uses the {@link BeanKeyCaptionColumnHeaderResolvingStrategy}
	 * 
	 * @return
	 */
	public ColumnConfigurationHeaderResolvingStrategyBuilder withBeanKeyCaptionStrategy()
	{
		return withStrategy(new BeanKeyCaptionColumnHeaderResolvingStrategy());
	}
	
	/**
	 * Uses the {@link ManualColumnHeaderResolvingStrategy}
	 * 
	 * @return
	 */
	public <I> ColumnConfigurationHeaderResolvingStrategyBuilder withManualColumnHeaderStrategy(
		final Function<Column<?>, I> identifierResolver,
		final Map<I, Function<I, String>> headerTextResolverMap)
	{
		return withStrategy(new ManualColumnHeaderResolvingStrategy<>(identifierResolver, headerTextResolverMap));
	}
	
	/**
	 * Adds a new {@link ColumnConfigHeaderResolvingStrategy}.<br/>
	 * This strategy will be added at the end of the strategy list.
	 * 
	 * @param strategy
	 * @return
	 */
	public ColumnConfigurationHeaderResolvingStrategyBuilder withStrategy(final ColumnHeaderResolvingStrategy strategy)
	{
		this.strategies.add(strategy);
		return this;
	}
	
	/**
	 * Adds a new {@link ColumnConfigHeaderResolvingStrategy}.<br/>
	 * This strategy will be added at the start of the strategy list.
	 * 
	 * @param strategy
	 * @return
	 */
	public ColumnConfigurationHeaderResolvingStrategyBuilder
		withFirstStrategy(final ColumnHeaderResolvingStrategy strategy)
	{
		this.strategies.add(0, strategy);
		return this;
	}
	
	/**
	 * Clears all existing strategies
	 *
	 * @return
	 */
	public ColumnConfigurationHeaderResolvingStrategyBuilder clearAllStrategies()
	{
		this.strategies.clear();
		return this;
	}
	
	public SerializableFunction<Column<?>, String> build()
	{
		return col -> {
			for(final ColumnHeaderResolvingStrategy resolvingFunction : this.strategies)
			{
				final Optional<String> optResolvedValue = resolvingFunction.resolve(col);
				if(optResolvedValue.isPresent())
				{
					return optResolvedValue.get();
				}
			}
			
			// Fallback
			if(col.getKey() != null)
			{
				return col.getKey();
			}
			return "";
		};
	}
}
