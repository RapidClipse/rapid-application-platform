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

import com.vaadin.flow.component.Component;
import com.vaadin.flow.function.SerializableSupplier;


/**
 * @author XDEV Software
 *
 */
public interface NavigationCategory extends NavigationElement
{
	public static NavigationCategory New(final String displayName)
	{
		return new Default(null, displayName);
	}
	
	public static NavigationCategory New(
		final SerializableSupplier<Component> icon,
		final String displayName)
	{
		return new Default(icon, displayName);
	}
	
	public static class Default extends NavigationElement.Abstract implements NavigationCategory
	{
		protected Default(
			final SerializableSupplier<Component> icon,
			final String displayName)
		{
			super(icon, displayName);
		}
		
		@Override
		public boolean equals(final Object obj)
		{
			return obj == this ||
				(obj instanceof NavigationCategory && ((NavigationCategory)obj).displayName().equals(displayName()));
		}

		@Override
		public int hashCode()
		{
			return displayName().hashCode();
		}
	}
}
