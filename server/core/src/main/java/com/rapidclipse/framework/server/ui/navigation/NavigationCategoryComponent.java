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
		return new Implementation(component, contentAdder);
	}
	
	public static <C extends Component & HasComponents> NavigationCategoryComponent New(final C component)
	{
		return new Implementation(component, component::add);
	}
	
	public static class Implementation implements NavigationCategoryComponent
	{
		private final Component             component;
		private final Consumer<Component[]> contentAdder;
		
		protected Implementation(final Component component, final Consumer<Component[]> contentAdder)
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
