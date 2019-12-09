/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
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
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 */
package com.rapidclipse.framework.server.navigation;

import java.util.function.Supplier;

import com.vaadin.flow.component.Component;
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
		final Supplier<Component> icon,
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
			final Supplier<Component> icon,
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
