/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
 *
 * This file is part of the RapidClipse Application Platform (RAP).
 *
 * RAP is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * RAP is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with RAP. If not, see <http://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 */
package com.rapidclipse.framework.server.ui.filter;

import java.io.Serializable;
import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.Set;

import com.rapidclipse.framework.server.util.ServiceLoader;


/**
 * @author XDEV Software
 *
 */
public interface SubsetDataProviderFactoryRegistry extends Serializable
{
	public SubsetDataProviderFactoryRegistry put(SubsetDataProviderFactory factory);

	public SubsetDataProviderFactoryRegistry remove(SubsetDataProviderFactory factory);

	public Collection<SubsetDataProviderFactory> getAll();

	public static SubsetDataProviderFactoryRegistry Empty()
	{
		return new Default();
	}

	public static SubsetDataProviderFactoryRegistry Default()
	{
		final SubsetDataProviderFactoryRegistry registry = Empty();

		ServiceLoader.forType(SubsetDataProviderFactory.class).servicesStream()
			.forEach(registry::put);

		return registry;
	}

	public static class Default implements SubsetDataProviderFactoryRegistry
	{
		private final Set<SubsetDataProviderFactory> factories = new LinkedHashSet<>();

		protected Default()
		{
			super();
		}

		@Override
		public SubsetDataProviderFactoryRegistry put(final SubsetDataProviderFactory factory)
		{
			this.factories.add(factory);

			return this;
		}

		@Override
		public SubsetDataProviderFactoryRegistry remove(final SubsetDataProviderFactory factory)
		{
			this.factories.remove(factory);

			return this;
		}

		@Override
		public Collection<SubsetDataProviderFactory> getAll()
		{
			return this.factories;
		}
	}
}
