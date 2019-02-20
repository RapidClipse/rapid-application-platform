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

package com.rapidclipse.framework.security.authentication;

/**
 * Type that provides a usable {@link Authenticator} instance.
 *
 * @author XDEV Software (TM)
 * 
 * @param <C>
 *            the type of the credentials instance to be authenticated.
 * @param <R>
 *            the type of the result/response instance to be returned upon an
 *            authentication attempt.
 */
public interface AuthenticatorProvider<C, R>
{
	/**
	 * Provides a usable {@link Authenticator} instance. Whether "providing"
	 * means creating a new instance or returning an existing one, maybe even
	 * always the same, is completely implementation-specific.
	 *
	 * @return a usable {@link Authenticator} instance
	 */
	public <T> Authenticator<C, R> provideAuthenticator();
}
