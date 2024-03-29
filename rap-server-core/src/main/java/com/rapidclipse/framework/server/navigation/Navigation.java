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
package com.rapidclipse.framework.server.navigation;

import static java.util.Objects.requireNonNull;

import java.lang.annotation.Annotation;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import com.rapidclipse.framework.server.util.ReflectionUtils;
import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.UI;
import com.vaadin.flow.router.BeforeEvent;
import com.vaadin.flow.router.QueryParameters;
import com.vaadin.flow.router.RouteConfiguration;
import com.vaadin.flow.router.RouteData;


/**
 * @author XDEV Software
 *
 */
public interface Navigation
{
	public static String ID_PARAMETER_NAME = "_";

	public static Navigation To(final Class<?> targetType)
	{
		return To(UI.getCurrent(), targetType);
	}

	public static Navigation To(final UI ui, final Class<?> targetType)
	{
		return new Default(ui, findComponentTargetType(targetType));
	}

	public static void navigateTo(final Class<?> targetType)
	{
		To(targetType).navigate();
	}

	public static void rerouteTo(final BeforeEvent event, final Class<?> targetType)
	{
		event.rerouteTo(findComponentTargetType(targetType));
	}

	@SuppressWarnings("unchecked")
	public static Class<? extends Component> findComponentTargetType(final Class<?> targetType)
	{
		final List<RouteData> routes      = RouteConfiguration.forSessionScope().getAvailableRoutes();
		final RouteData       targetRoute =
			targetType.isAnnotation()
				? routes.stream()
					.filter(data -> ReflectionUtils.isAnnotationPresent(data.getNavigationTarget(),
						(Class<? extends Annotation>)targetType))
					.findAny().orElse(null)
				: routes.stream()
					.filter(data -> targetType.equals(data.getNavigationTarget()))
					.findAny()
					.orElse(routes.stream()
						.filter(data -> targetType.isAssignableFrom(data.getNavigationTarget()))
						.findAny()
						.orElse(null));
		if(targetRoute != null)
		{
			return targetRoute.getNavigationTarget();
		}

		throw new NavigationException("No route found for: " + targetType.getCanonicalName());
	}

	public Navigation withParameter(String name, final Object value);

	public void navigate();

	public static class Default implements Navigation
	{
		private final UI                           ui;
		private final Class<? extends Component>   targetType;
		private final NavigationParametersMetadata metadata;
		private final Map<String, Object>          parameters;

		protected Default(final UI ui, final Class<? extends Component> targetType)
		{
			super();
			this.ui         = requireNonNull(ui);
			this.targetType = requireNonNull(targetType);
			this.metadata   = NavigationParametersMetadata.New(targetType);
			this.parameters = new HashMap<>();
		}

		@Override
		public Navigation withParameter(String name, final Object value)
		{
			name = requireNonNull(name).toLowerCase();

			final NavigationParameterMetadata paramMetadata = this.metadata.get(name);
			if(paramMetadata == null)
			{
				throw new IllegalArgumentException("Parameter " + this.targetType.getCanonicalName()
					+ "#" + name + " not found");
			}

			if(value != null && !paramMetadata.type().isInstance(value))
			{
				throw new IllegalArgumentException(name + " = " + value);
			}

			this.parameters.put(name, value);

			return this;
		}

		@Override
		public void navigate()
		{
			final List<String> mandatoryParameters = this.metadata.mandatoryParameters();
			mandatoryParameters.removeAll(this.parameters.keySet());
			if(mandatoryParameters.size() > 0)
			{
				throw new NavigationException("Missing parameters: "
					+ mandatoryParameters.stream().collect(Collectors.joining(", ")));
			}

			if(HasNavigationParameters.class.isAssignableFrom(this.targetType))
			{
				final NavigationParameterRegistry registry = NavigationParameterRegistry
					.getCurrent();
				final String                      id       = registry.put(NavigationParameters.New(this.parameters));

				final Map<String, String> paramMap = new HashMap<>();
				paramMap.put(ID_PARAMETER_NAME, id);

				// _ as dummy parameter value
				final String url = RouteConfiguration.forSessionScope().getUrl(targetTypeWithParameters(), "_");

				this.ui.navigate(url, QueryParameters.simple(paramMap));
			}
			else
			{
				this.ui.navigate(this.targetType);
			}
		}

		@SuppressWarnings("unchecked") // Type-safety ensured by condition
		private <T extends Component & HasNavigationParameters> Class<T> targetTypeWithParameters()
		{
			return (Class<T>)this.targetType;
		}
	}
}
