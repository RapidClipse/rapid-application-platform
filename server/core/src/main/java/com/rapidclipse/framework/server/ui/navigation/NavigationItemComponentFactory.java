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
