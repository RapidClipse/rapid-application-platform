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
