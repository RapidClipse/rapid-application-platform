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
