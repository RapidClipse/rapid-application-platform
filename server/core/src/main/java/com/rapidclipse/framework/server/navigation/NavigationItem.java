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
		return new Implementation(icon, displayName, routeData, position, category);
	}
	
	public static class Implementation extends NavigationElement.Abstract implements NavigationItem
	{
		private final RouteData routeData;
		private final int       position;
		private final String    category;
		
		protected Implementation(
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
