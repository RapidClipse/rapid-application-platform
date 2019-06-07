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

package com.rapidclipse.framework.server.navigation;

import static java.util.Objects.requireNonNull;

import java.util.Comparator;
import java.util.List;
import java.util.Objects;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.router.RouteConfiguration;
import com.vaadin.flow.router.RouteData;


/**
 * @author XDEV Software
 *
 */
@FunctionalInterface
public interface NavigationItemProvider
{
	public List<NavigationItem> getItems();

	public static Comparator<NavigationItem> PositionSorter()
	{
		return (item1, item2) -> {
			final int pos1 = item1.position();
			final int pos2 = item2.position();
			return pos1 == pos2 ? 0 : pos1 < 0 ? 1 : pos2 < 0 ? -1 : pos1 < pos2 ? -1 : 1;
		};
	}

	public static Comparator<NavigationItem> LexicalSorter()
	{
		return (item1, item2) -> item1.displayName().compareTo(item2.displayName());
	}

	public static NavigationItemProvider New()
	{
		return new Implementation(NavigationItemFilter.RegisteredFilters(), PositionSorter());
	}

	public static NavigationItemProvider New(final Predicate<NavigationItem> itemFilter)
	{
		return new Implementation(itemFilter, PositionSorter());
	}

	public static NavigationItemProvider New(final Comparator<NavigationItem> itemSorter)
	{
		return new Implementation(NavigationItemFilter.RegisteredFilters(), itemSorter);
	}

	public static NavigationItemProvider
		New(final Predicate<NavigationItem> itemFilter, final Comparator<NavigationItem> itemSorter)
	{
		return new Implementation(itemFilter, itemSorter);
	}

	public static class Implementation implements NavigationItemProvider
	{
		private final Predicate<NavigationItem>  itemFilter;
		private final Comparator<NavigationItem> itemSorter;

		public Implementation(final Predicate<NavigationItem> itemFilter, final Comparator<NavigationItem> itemSorter)
		{
			super();

			this.itemFilter = requireNonNull(itemFilter);
			this.itemSorter = requireNonNull(itemSorter);
		}

		@Override
		public List<NavigationItem> getItems()
		{
			return RouteConfiguration.forSessionScope().getAvailableRoutes().stream()
				.map(this::toItem)
				.filter(Objects::nonNull)
				.filter(this.itemFilter)
				.sorted(this.itemSorter)
				.collect(Collectors.toList());
		}

		protected NavigationItem toItem(final RouteData data)
		{
			final Class<? extends Component> target               = data.getNavigationTarget();
			final NavigationItemProperties   propertiesAnnotation =
				target.getAnnotation(NavigationItemProperties.class);
			if(propertiesAnnotation == null)
			{
				return null;
			}

			final int                position       = propertiesAnnotation.position();
			final String             category       = propertiesAnnotation.category();
			final NavigationItemIcon iconAnnotation = target.getAnnotation(NavigationItemIcon.class);
			final VaadinIcon         icon           = iconAnnotation != null ? iconAnnotation.value() : null;
			String                   displayName    = propertiesAnnotation.displayName();
			if(StringUtils.isEmpty(displayName))
			{
				displayName = target.getSimpleName();
			}

			return NavigationItem.New(data, position, category, icon, displayName);
		}
	}
}
