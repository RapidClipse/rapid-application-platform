/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.navigation;

import static java.util.Collections.unmodifiableMap;
import static java.util.Objects.requireNonNull;

import java.io.Serializable;
import java.util.Map;


/**
 * @author XDEV Software
 *
 */
public interface NavigationParameters extends Serializable
{
	public Iterable<String> names();

	public Object value(final String name);

	public static NavigationParameters New(final Map<String, Object> mapping)
	{
		return new Default(mapping);
	}

	public static class Default implements NavigationParameters
	{
		private final Map<String, Object> mapping;

		protected Default(final Map<String, Object> mapping)
		{
			super();
			this.mapping = unmodifiableMap(requireNonNull(mapping));
		}

		@Override
		public Iterable<String> names()
		{
			return this.mapping.keySet();
		}

		@Override
		public Object value(final String name)
		{
			return this.mapping.get(name);
		}
	}
}
