/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
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
