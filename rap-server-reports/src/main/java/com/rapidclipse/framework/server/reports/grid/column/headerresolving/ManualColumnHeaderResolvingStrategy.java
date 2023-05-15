/*
 * Copyright (C) 2013-2023 by XDEV Software, All Rights Reserved.
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

package com.rapidclipse.framework.server.reports.grid.column.headerresolving;

import java.util.Map;
import java.util.Optional;
import java.util.function.Function;

import com.vaadin.flow.component.grid.Grid.Column;


/**
 * Resolves the header-text by using the unique identification {@linkplain I}
 * and find the corresponding resolving function for the identification in a map.<br/>
 * This function is the used to resolve the header-text.<br/>
 * <br/>
 * Example:<br/>
 *
 * <pre>
 * new ManualColumnHeaderResolvingStrategy(
 * 	col -> col.getKey(),
 * 	Map.of(
 * 		"name", "Username",
 * 		"pw", "Password"))
 * </pre>
 * 
 * @author XDEV Software
 *
 * @param <I>
 *            The identifier of the column, e.g. the key or the column itself
 */
public class ManualColumnHeaderResolvingStrategy<I> implements ColumnHeaderResolvingStrategy
{
	private final Function<Column<?>, I>      identifierResolver;
	private final Map<I, Function<I, String>> headerTextResolverMap;
	
	public ManualColumnHeaderResolvingStrategy(
		final Function<Column<?>, I> identifierResolver,
		final Map<I, Function<I, String>> headerTextResolverMap)
	{
		this.identifierResolver    = identifierResolver;
		this.headerTextResolverMap = headerTextResolverMap;
	}
	
	@Override
	public Optional<String> resolve(final Column<?> column)
	{
		final I                   identifier      = this.identifierResolver.apply(column);
		
		final Function<I, String> resolveFunction = this.headerTextResolverMap.get(identifier);
		if(resolveFunction == null)
		{
			return Optional.empty();
		}
		
		return Optional.ofNullable(resolveFunction.apply(identifier));
	}
	
}
