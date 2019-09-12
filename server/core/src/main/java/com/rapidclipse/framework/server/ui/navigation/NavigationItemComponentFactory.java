/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
 *
 * This file is part of the RapidClipse Application Platform (RAP).
 *
 * RAP is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * RAP is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with RAP. If not, see <http://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 */
package com.rapidclipse.framework.server.ui.navigation;

import java.io.Serializable;
import java.util.Objects;
import java.util.Optional;

import com.rapidclipse.framework.server.navigation.NavigationItem;
import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.button.ButtonVariant;
import com.vaadin.flow.function.SerializableFunction;
import com.vaadin.flow.router.RouterLink;


/**
 * @author XDEV Software
 *
 */
@FunctionalInterface
public interface NavigationItemComponentFactory extends SerializableFunction<NavigationItem, Component>
{
	@FunctionalInterface
	public static interface EventHandler extends Serializable
	{
		public void handleEvent(Component source, NavigationItem item);
		
		public static EventHandler Default()
		{
			return (source, item) -> UI.getCurrent().navigate(item.routeData().getNavigationTarget());
		}
	}

	public static NavigationItemComponentFactory LinkFactory()
	{
		return new LinkFactory();
	}

	public static NavigationItemComponentFactory ButtonFactory()
	{
		return new ButtonFactory(EventHandler.Default());
	}

	public static NavigationItemComponentFactory ButtonFactory(final EventHandler eventHandler)
	{
		return new ButtonFactory(eventHandler);
	}

	public static class LinkFactory implements NavigationItemComponentFactory
	{
		protected LinkFactory()
		{
			super();
		}

		@Override
		public Component apply(final NavigationItem item)
		{
			return new RouterLink(item.displayName(), item.routeData().getNavigationTarget());
		}
	}

	public static class ButtonFactory implements NavigationItemComponentFactory
	{
		private final EventHandler eventHandler;

		protected ButtonFactory(final EventHandler eventHandler)
		{
			super();

			this.eventHandler = Objects.requireNonNull(eventHandler);
		}

		@Override
		public Component apply(final NavigationItem item)
		{
			final Button button = new Button(item.displayName());
			button.addThemeVariants(ButtonVariant.LUMO_SMALL);
			Optional.ofNullable(item.icon()).ifPresent(icon -> button.setIcon(icon.get()));
			button.addClickListener(event -> this.eventHandler.handleEvent(button, item));
			return button;
		}
	}
}
