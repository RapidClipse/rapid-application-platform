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
package com.rapidclipse.framework.server.ui.filter;

import java.io.Serializable;
import java.util.function.BiPredicate;
import java.util.function.Predicate;


/**
 * @author XDEV Software
 *
 */
@FunctionalInterface
public interface SubsetDataProviderFactory extends Serializable
{
	public SubsetDataProvider<?> createFor(
		final FilterContext context,
		final FilterProperty<?> property);
	
	public static <T> SubsetDataProviderFactory New(
		final Class<T> type,
		final SubsetDataProvider<T> provider)
	{
		return New(property -> type.equals(property.type()), provider);
	}
	
	public static SubsetDataProviderFactory New(
		final BiPredicate<FilterContext, FilterProperty<?>> predicate,
		final SubsetDataProvider<?> provider)
	{
		return (context, property) -> predicate.test(context, property) ? provider : null;
	}
	
	public static SubsetDataProviderFactory New(
		final Predicate<FilterProperty<?>> predicate,
		final SubsetDataProvider<?> provider)
	{
		return (context, property) -> predicate.test(property) ? provider : null;
	}
}
