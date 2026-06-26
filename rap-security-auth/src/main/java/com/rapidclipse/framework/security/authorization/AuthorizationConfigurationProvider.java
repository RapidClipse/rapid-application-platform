/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.security.authorization;

/**
 * Function type that provides {@link AuthorizationConfiguration} instances.
 *
 * @author XDEV Software (TM)
 */
@FunctionalInterface
public interface AuthorizationConfigurationProvider
{
	/**
	 * Provides an authorization configuration in generic form from one or more sources.
	 *
	 * @return an authorization configuration instance.
	 */
	public AuthorizationConfiguration provideConfiguration();
}
