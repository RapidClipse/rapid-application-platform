/*
 * Copyright (C) 2013-2021 by XDEV Software, All Rights Reserved.
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

import static java.util.Objects.requireNonNull;

import java.io.Serializable;
import java.util.function.Supplier;

import com.vaadin.flow.component.Component;


/**
 * @author XDEV Software
 *
 */
public interface NavigationElement extends Serializable
{
	public Supplier<Component> icon();

	public String displayName();

	public static abstract class Abstract implements NavigationElement
	{
		private final Supplier<Component> icon;
		private final String              displayName;

		public Abstract(
			final Supplier<Component> icon,
			final String displayName)
		{
			super();

			this.icon        = icon;
			this.displayName = requireNonNull(displayName);
		}

		@Override
		public Supplier<Component> icon()
		{
			return this.icon;
		}

		@Override
		public String displayName()
		{
			return this.displayName;
		}

		@Override
		public String toString()
		{
			return this.displayName;
		}
	}
}
