/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
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
