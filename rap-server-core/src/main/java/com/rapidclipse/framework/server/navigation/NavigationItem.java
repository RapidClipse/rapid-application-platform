/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.navigation;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.function.SerializableSupplier;
import com.vaadin.flow.router.RouteData;


/**
 * @author XDEV Software
 *
 */
public interface NavigationItem extends NavigationElement
{
	public RouteData routeData();

	public int position();

	public String category();

	public boolean isHidden();

	public static NavigationItem New(
		final SerializableSupplier<Component> icon,
		final String displayName,
		final RouteData routeData,
		final int position,
		final String category,
		final boolean hidden)
	{
		return new Default(icon, displayName, routeData, position, category, hidden);
	}

	public static class Default extends NavigationElement.Abstract implements NavigationItem
	{
		private final RouteData routeData;
		private final int       position;
		private final String    category;
		private final boolean   hidden;

		protected Default(
			final SerializableSupplier<Component> icon,
			final String displayName,
			final RouteData routeData,
			final int position,
			final String category,
			final boolean hidden)
		{
			super(icon, displayName);

			this.routeData = routeData;
			this.position  = position;
			this.category  = category;
			this.hidden    = hidden;
		}

		@Override
		public RouteData routeData()
		{
			return this.routeData;
		}

		@Override
		public int position()
		{
			return this.position;
		}

		@Override
		public String category()
		{
			return this.category;
		}

		@Override
		public boolean isHidden()
		{
			return this.hidden;
		}
	}
}
