/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
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
