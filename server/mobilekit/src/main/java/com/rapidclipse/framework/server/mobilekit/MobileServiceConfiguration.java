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

import java.util.Map;


/**
 * @author XDEV Software
 *
 */
public interface MobileServiceConfiguration
{
	public Class<? extends MobileService> getServiceClass();
	
	public Map<String, String> getParameters();
	
	public static MobileServiceConfiguration New(
		final Class<? extends MobileService> serviceClass,
		final Map<String, String> params)
	{
		return new Implementation(serviceClass, params);
	}
	
	public static class Implementation implements MobileServiceConfiguration
	{
		private final Class<? extends MobileService> serviceClass;
		private final Map<String, String>            params;
		
		public Implementation(
			final Class<? extends MobileService> serviceClass,
			final Map<String, String> params)
		{
			super();
			
			this.serviceClass = serviceClass;
			this.params       = params;
		}
		
		@Override
		public Class<? extends MobileService> getServiceClass()
		{
			return this.serviceClass;
		}
		
		@Override
		public Map<String, String> getParameters()
		{
			return this.params;
		}
	}
}
