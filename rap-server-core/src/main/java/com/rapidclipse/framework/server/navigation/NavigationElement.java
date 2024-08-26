/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.navigation;

import static java.util.Objects.requireNonNull;

import java.io.Serializable;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.function.SerializableSupplier;


/**
 * @author XDEV Software
 *
 */
public interface NavigationElement extends Serializable
{
	public SerializableSupplier<Component> icon();

	public String displayName();

	public static abstract class Abstract implements NavigationElement
	{
		private final SerializableSupplier<Component> icon;
		private final String              displayName;

		public Abstract(
			final SerializableSupplier<Component> icon,
			final String displayName)
		{
			super();

			this.icon        = icon;
			this.displayName = requireNonNull(displayName);
		}

		@Override
		public SerializableSupplier<Component> icon()
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
