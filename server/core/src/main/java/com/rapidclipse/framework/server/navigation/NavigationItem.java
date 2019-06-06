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

import com.vaadin.flow.component.icon.VaadinIcon;


/**
 * @author XDEV Software
 *
 */
public interface NavigationItem
{
	public int position();

	public String category();

	public VaadinIcon icon();

	public String displayName();
	
	public static NavigationItem New(
		final int position,
		final String category,
		final VaadinIcon icon,
		final String displayName)
	{
		return new Implementation(position, category, icon, displayName);
	}

	public static class Implementation implements NavigationItem
	{
		private final int        position;
		private final String     category;
		private final VaadinIcon icon;
		private final String     displayName;

		public Implementation(
			final int position,
			final String category,
			final VaadinIcon icon,
			final String displayName)
		{
			super();

			this.position    = position;
			this.category    = category;
			this.icon        = icon;
			this.displayName = displayName;
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
		public VaadinIcon icon()
		{
			return this.icon;
		}
		
		@Override
		public String displayName()
		{
			return this.displayName;
		}
	}
}
