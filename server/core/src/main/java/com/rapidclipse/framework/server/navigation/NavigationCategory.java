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
		final Supplier<Component> icon,
		final String displayName)
	{
		return new Default(icon, displayName);
	}
	
	public static class Default extends NavigationElement.Abstract implements NavigationCategory
	{
		protected Default(
			final Supplier<Component> icon,
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
