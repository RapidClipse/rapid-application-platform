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
package com.rapidclipse.framework.server.ui;

import java.util.Objects;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Predicate;

import com.vaadin.flow.component.Component;


/**
 * @author XDEV Software
 *
 */
public final class UIUtils
{
	@SuppressWarnings("unchecked") // type ensured by instance check
	public static <T> T getNextParent(final Component c, final Class<T> type)
	{
		return (T)getNextParent(c, type::isInstance);
	}

	public static Component getNextParent(final Component c, final Predicate<Component> predicate)
	{
		Component parent = c;
		while(parent != null)
		{
			if(predicate.test(parent))
			{
				return parent;
			}

			parent = parent.getParent().orElse(null);
		}

		return null;
	}

	/**
	 *
	 * @param parent
	 * @param visitor
	 */
	public static void traverseComponentTree(
		final Component parent,
		final Consumer<Component> visitor)
	{
		lookupComponentTree(parent, toFunction(visitor));
	}

	/**
	 *
	 * @param parent
	 * @param visitor
	 * @param type
	 */
	public static <C> void traverseComponentTree(
		final Component parent,
		final Class<C> type,
		final Consumer<C> visitor)
	{
		lookupComponentTree(parent, type, toFunction(visitor));
	}

	private static <C, T> Function<C, T> toFunction(final Consumer<C> consumer)
	{
		return c -> {
			consumer.accept(c);
			return null;
		};
	}

	/**
	 * Shortcut for <code>lookupComponentTree(parent,visitor,null)</code>.
	 *
	 * @param <T>
	 *            The return type
	 * @param parent
	 *            The root of the component tree to visit
	 * @param visitor
	 *            the visitor
	 * @return
	 * @see #lookupComponentTree(Component, Class, Function)
	 */

	public static <T> T lookupComponentTree(
		final Component parent,
		final Function<Component, T> visitor)
	{
		return lookupComponentTree(parent, null, visitor);
	}

	/**
	 * Walks through the <code>parent</code>'s component tree hierarchy.
	 * <p>
	 * For every child in the tree hierarchy, which fits the <code>type</code>
	 * parameter (or type is <code>null</code>), the <code>visitor</code> is
	 * invoked.
	 * <p>
	 * If the visitor returns a value != <code>null</code>, the visitation ends
	 * and that value is returned.
	 *
	 *
	 * @param <T>
	 *            The return type
	 * @param <C>
	 *            The component type to visit
	 * @param parent
	 *            The root of the component tree to visit
	 * @param type
	 *            The component type class to visit
	 * @param visitor
	 *            the visitor
	 * @return
	 * @see {@link ComponentTreeVisitor}
	 */

	@SuppressWarnings("unchecked")
	public static <C, T> T lookupComponentTree(
		final Component parent,
		final Class<C> type,
		final Function<C, T> visitor)
	{
		T value = null;

		if(type == null || type.isInstance(parent))
		{
			value = visitor.apply((C)parent);
		}

		if(value == null)
		{
			value = parent.getChildren()
				.map(child -> lookupComponentTree(child, type, visitor))
				.filter(Objects::nonNull)
				.findFirst()
				.orElse(null);
		}

		return value;
	}

	private UIUtils()
	{
		throw new Error();
	}
}
