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

package software.xdev.rap.server.ui;


import java.util.Objects;
import java.util.function.Consumer;
import java.util.function.Function;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.HasComponents;


/**
 * @author XDEV Software
 *
 */
public final class UIUtils
{
	public static <T> T getNextParent(final Component c, final Class<T> type)
	{
		Component parent = c;
		while(parent != null)
		{
			if(type.isInstance(parent))
			{
				return type.cast(parent);
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
	public static void traverseComponentTree(final Component parent,
			final Consumer<Component> visitor)
	{
		lookupComponentTree(parent,toFunction(visitor));
	}


	/**
	 *
	 * @param parent
	 * @param visitor
	 * @param type
	 */
	public static <C extends Component> void traverseComponentTree(final Component parent,
			final Consumer<C> visitor, final Class<C> type)
	{
		lookupComponentTree(parent,toFunction(visitor),type);
	}


	private static <C extends Component, T> Function<C, T> toFunction(final Consumer<C> consumer)
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
	 * @see #lookupComponentTree(Component, Function, Class)
	 */

	public static <T> T lookupComponentTree(final Component parent,
			final Function<Component, T> visitor)
	{
		return lookupComponentTree(parent,visitor,null);
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
	 * @param visitor
	 *            the visitor
	 * @param type
	 *            The component type class to visit
	 * @return
	 * @see {@link ComponentTreeVisitor}
	 */

	@SuppressWarnings("unchecked")
	public static <C extends Component, T> T lookupComponentTree(final Component parent,
			final Function<C, T> visitor, final Class<C> type)
	{
		T value = null;

		if(type == null || type.isInstance(parent))
		{
			value = visitor.apply((C)parent);
		}

		if(value == null)
		{
			value = parent.getChildren().map(child -> traverse(child,visitor,type))
					.filter(Objects::nonNull).findFirst().orElse(null);
		}

		return value;
	}


	@SuppressWarnings("unchecked")
	private static <C extends Component, T> T traverse(final Component child,
			final Function<C, T> visitor, final Class<C> type)
	{
		if(child instanceof HasComponents)
		{
			return lookupComponentTree(child,visitor,type);
		}

		if(type == null || type.isInstance(child))
		{
			return visitor.apply((C)child);
		}

		return null;
	}
	
	
	private UIUtils()
	{
		throw new Error();
	}
}
