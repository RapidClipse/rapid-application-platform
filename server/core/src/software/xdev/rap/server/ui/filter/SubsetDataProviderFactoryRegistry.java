/*
 * Copyright (C) 2013-2018 by XDEV Software, All Rights Reserved.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 *
 * For further information see
 * <http://www.rapidclipse.com/en/legal/license/license.html>.
 */

package software.xdev.rap.server.ui.filter;


import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.Set;

import software.xdev.rap.server.util.ServiceLoader;


/**
 * @author XDEV Software
 *
 */
public interface SubsetDataProviderFactoryRegistry
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
