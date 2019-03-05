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
	
	public static SubsetDataProviderFactoryRegistry New()
	{
		return new Implementation();
	}
	
	public static SubsetDataProviderFactoryRegistry Default()
	{
		final SubsetDataProviderFactoryRegistry registry = New();
		
		ServiceLoader.forType(SubsetDataProviderFactory.class).servicesStream()
			.forEach(registry::put);
		
		return registry;
	}
	
	public static class Implementation implements SubsetDataProviderFactoryRegistry
	{
		private final Set<SubsetDataProviderFactory> factories = new LinkedHashSet<>();
		
		public Implementation()
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
