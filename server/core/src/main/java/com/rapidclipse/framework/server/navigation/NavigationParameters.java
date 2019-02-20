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
		return new Implementation(mapping);
	}
	
	public static class Implementation implements NavigationParameters
	{
		private final Map<String, Object> mapping;
		
		public Implementation(final Map<String, Object> mapping)
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
