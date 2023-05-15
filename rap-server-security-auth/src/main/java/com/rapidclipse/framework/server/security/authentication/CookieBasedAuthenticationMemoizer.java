/*
 * Copyright (C) 2013-2022 by XDEV Software, All Rights Reserved.
 *
 * This file is part of the RapidClipse Application Platform (RAP).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software - initial API and implementation
 */
package com.rapidclipse.framework.server.security.authentication;

import java.time.Duration;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import com.rapidclipse.framework.security.authentication.AuthenticationMemoizer;
import com.rapidclipse.framework.security.authentication.CredentialsUsernamePassword;
import com.rapidclipse.framework.server.Rap;
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
	public static CookieBasedAuthenticationMemoizer getCurrent()
	{
		return Rap.ensureSessionInstance(CookieBasedAuthenticationMemoizer.class,
			session -> new CookieBasedAuthenticationMemoizer());
	}
	
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
