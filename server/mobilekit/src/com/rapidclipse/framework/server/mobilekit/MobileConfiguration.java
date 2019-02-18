/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
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

package com.rapidclipse.framework.server.mobilekit;


import java.util.Collection;


/**
 * @author XDEV Software
 *
 */
public interface MobileConfiguration
{
	public Collection<MobileServiceConfiguration> getMobileServices();


	public static MobileConfiguration New(
			final Collection<MobileServiceConfiguration> mobileServices)
	{
		return new Implementation(mobileServices);
	}
	
	
	
	public static class Implementation implements MobileConfiguration
	{
		private final Collection<MobileServiceConfiguration> mobileServices;


		public Implementation(final Collection<MobileServiceConfiguration> mobileServices)
		{
			super();

			this.mobileServices = mobileServices;
		}


		@Override
		public Collection<MobileServiceConfiguration> getMobileServices()
		{
			return this.mobileServices;
		}
	}
}
