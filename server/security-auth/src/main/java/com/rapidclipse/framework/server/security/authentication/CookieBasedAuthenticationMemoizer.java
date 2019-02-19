/*-
 * ---
 * Rapid Application Platform / Server / Security / Authentication and Authorization
 * --
 * Copyright (C) 2013 - 2019 XDEV Software Corp.
 * --
 * This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * which accompanies this distribution, and is available at
 * https://www.eclipse.org/legal/epl-2.0/
 * 
 * SPDX-License-Identifier: EPL-2.0
 * 
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 * ---
 */

package com.rapidclipse.framework.server.security.authentication;

import java.time.Duration;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import com.rapidclipse.framework.security.authentication.AuthenticationMemoizer;
import com.rapidclipse.framework.security.authentication.CredentialsUsernamePassword;
import com.rapidclipse.framework.server.net.Cookies;


/**
 * @author XDEV Software
 *
 */
/**
 * Cookie based implementation of {@link AuthenticationMemoizer}.
 *
 * @see Cookies
 *
 * @author XDEV Software
 */
public class CookieBasedAuthenticationMemoizer
	implements AuthenticationMemoizer<CredentialsUsernamePassword>
{
	protected final static String                            COOKIE_NAME = "XUID";
	protected final Map<String, CredentialsUsernamePassword> rememberedCredentials;
	protected Duration                                       lifespan;
	
	public CookieBasedAuthenticationMemoizer()
	{
		this.rememberedCredentials = new HashMap<>();
		this.lifespan              = Duration.ofDays(14);
	}
	
	public Duration getLifespan()
	{
		return this.lifespan;
	}
	
	public void setLifespan(final Duration lifespan)
	{
		this.lifespan = lifespan;
	}
	
	@Override
	public void remember(final CredentialsUsernamePassword credentials)
	{
		remove(credentials);
		
		final String hash = UUID.randomUUID().toString();
		Cookies.getCurrent().setCookie(COOKIE_NAME, hash, this.lifespan);
		this.rememberedCredentials.put(hash, credentials);
	}
	
	@Override
	public void forget(final CredentialsUsernamePassword credentials)
	{
		remove(credentials);
		
		Cookies.getCurrent().deleteCookie(COOKIE_NAME);
	}
	
	protected void remove(final CredentialsUsernamePassword credentials)
	{
		this.rememberedCredentials.values().remove(credentials);
	}
	
	@Override
	public CredentialsUsernamePassword lookup()
	{
		final String hash = Cookies.getCurrent().getCookie(COOKIE_NAME);
		return this.rememberedCredentials.get(hash);
	}
}
