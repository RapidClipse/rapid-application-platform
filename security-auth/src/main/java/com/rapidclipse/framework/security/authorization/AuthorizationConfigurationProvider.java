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
