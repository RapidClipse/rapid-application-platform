/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
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
