/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.navigation;

import static java.util.Objects.requireNonNull;

import java.io.Serializable;
import java.lang.annotation.Annotation;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.List;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;

import com.rapidclipse.framework.server.resources.CaptionUtils;
import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.icon.IconFactory;
import com.vaadin.flow.function.SerializableComparator;
import com.vaadin.flow.function.SerializablePredicate;
import com.vaadin.flow.function.SerializableSupplier;
import com.vaadin.flow.router.PageTitle;
import com.vaadin.flow.router.RouteConfiguration;
import com.vaadin.flow.router.RouteData;


/**
 * @author XDEV Software
 *
 */
@FunctionalInterface
public interface NavigationItemProvider extends Serializable
{
	public List<NavigationItem> getItems();

	public static SerializableComparator<NavigationItem> PositionSorter()
	{
		return (item1, item2) -> {
			final int pos1 = item1.position();
			final int pos2 = item2.position();
			return pos1 == pos2 ? 0 : pos1 < 0 ? 1 : pos2 < 0 ? -1 : pos1 < pos2 ? -1 : 1;
		};
	}

	public static SerializableComparator<NavigationItem> LexicalSorter()
	{
		return (item1, item2) -> item1.displayName().compareTo(item2.displayName());
	}

	public static NavigationItemProvider New()
	{
		return new Default(NavigationItemFilter.RegisteredFilters(), PositionSorter());
	}

	public static NavigationItemProvider New(final SerializablePredicate<NavigationItem> itemFilter)
	{
		return new Default(itemFilter, PositionSorter());
	}

	public static NavigationItemProvider New(final SerializableComparator<NavigationItem> itemSorter)
	{
		return new Default(NavigationItemFilter.RegisteredFilters(), itemSorter);
	}

	public static NavigationItemProvider
		New(
			final SerializablePredicate<NavigationItem> itemFilter,
			final SerializableComparator<NavigationItem> itemSorter)
	{
		return new Default(itemFilter, itemSorter);
	}

	public static class Default implements NavigationItemProvider
	{
		private final SerializablePredicate<NavigationItem>  itemFilter;
		private final SerializableComparator<NavigationItem> itemSorter;

		protected Default(
			final SerializablePredicate<NavigationItem> itemFilter,
			final SerializableComparator<NavigationItem> itemSorter)
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
				.filter(this.itemFilter)
				.sorted(this.itemSorter)
				.collect(Collectors.toList());
		}

		protected NavigationItem toItem(final RouteData data)
		{
			final Class<? extends Component> target = data.getNavigationTarget();

			final NavigationItemProperties propertiesAnnotation =
				target.getAnnotation(NavigationItemProperties.class);

			final SerializableSupplier<Component> icon        = resolveIcon(target);
			String                    displayName = null;
			int                       position    = -1;
			String                    category    = null;
			boolean                   hidden      = false;

			if(propertiesAnnotation != null)
			{
				displayName = propertiesAnnotation.displayName();
				position    = propertiesAnnotation.position();
				category    = propertiesAnnotation.category();
				hidden      = propertiesAnnotation.hidden();
			}

			if(StringUtils.isEmpty(displayName))
			{
				final PageTitle pageTitle = target.getAnnotation(PageTitle.class);
				if(pageTitle != null)
				{
					displayName = pageTitle.value();
				}
				else
				{
					displayName = CaptionUtils.resolveCaption(target);
				}
			}

			return NavigationItem.New(icon, displayName, data, position, category, hidden);
		}

		protected SerializableSupplier<Component> resolveIcon(final Class<? extends Component> target)
		{
			final NavigationIconFactory factory = target.getAnnotation(NavigationIconFactory.class);
			if(factory != null)
			{
				try
				{
					return factory.value().getDeclaredConstructor().newInstance();
				}
				catch(InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException | NoSuchMethodException | SecurityException e)
				{
					throw new RuntimeException(e);
				} 
			}

			for(final Annotation annotation : target.getAnnotations())
			{
				final Class<? extends Annotation> annotationType = annotation.annotationType();
				if(annotationType.getAnnotation(NavigationIcon.class) != null)
				{
					try
					{
						final Method valueMethod = annotationType.getDeclaredMethod("value");
						final Object value       = valueMethod.invoke(annotation);
						if(value instanceof IconFactory)
						{
							return ((IconFactory)value)::create;
						}
					}
					catch(IllegalAccessException | IllegalArgumentException | InvocationTargetException
						| NoSuchMethodException | SecurityException e)
					{
						throw new RuntimeException(e);
					}
				}
			}

			return null;
		}
	}
}
