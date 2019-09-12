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

	public static NavigationItem New(
		final Supplier<Component> icon,
		final String displayName,
		final RouteData routeData,
		final int position,
		final String category)
	{
		return new Default(icon, displayName, routeData, position, category);
	}

	public static class Default extends NavigationElement.Abstract implements NavigationItem
	{
		private final RouteData routeData;
		private final int       position;
		private final String    category;

		protected Default(
			final Supplier<Component> icon,
			final String displayName,
			final RouteData routeData,
			final int position,
			final String category)
		{
			super(icon, displayName);

			this.routeData = routeData;
			this.position  = position;
			this.category  = category;
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
	}
}
