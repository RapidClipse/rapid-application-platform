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
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 */
package com.rapidclipse.framework.server.ui.navigation;

import java.io.Serializable;
import java.util.function.Consumer;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.HasComponents;


/**
 * @author XDEV Software
 *
 */
public interface NavigationCategoryComponent extends Serializable
{
	public Component component();
	
	public Consumer<Component[]> contentAdder();

	public static NavigationCategoryComponent New(final Component component, final Consumer<Component[]> contentAdder)
	{
		return new Default(component, contentAdder);
	}

	public static <C extends Component & HasComponents> NavigationCategoryComponent New(final C component)
	{
		return new Default(component, component::add);
	}

	public static class Default implements NavigationCategoryComponent
	{
		private final Component             component;
		private final Consumer<Component[]> contentAdder;

		protected Default(final Component component, final Consumer<Component[]> contentAdder)
		{
			super();
			
			this.component    = component;
			this.contentAdder = contentAdder;
		}

		@Override
		public Component component()
		{
			return this.component;
		}

		@Override
		public Consumer<Component[]> contentAdder()
		{
			return this.contentAdder;
		}
	}
}
