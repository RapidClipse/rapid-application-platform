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
