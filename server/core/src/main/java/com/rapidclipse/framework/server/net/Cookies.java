/*
 * Copyright (C) 2013-2021 by XDEV Software, All Rights Reserved.
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
 *     XDEV Software Corp. - initial API and implementation
 */
package com.rapidclipse.framework.server.net;

import java.io.Serializable;
import java.time.Duration;
import java.util.Arrays;

import javax.servlet.http.Cookie;

import com.vaadin.flow.component.UI;
import com.vaadin.flow.server.VaadinSession;


/**
 * @author XDEV Software
 *
 */
public interface Cookies extends Serializable
{
	public default void setCookie(final String key, final String value)
	{
		setCookie(key, value, "/", null);
	}
	
	public default void setCookie(final String key, final String value, final Duration lifespan)
	{
		setCookie(key, value, "/", lifespan);
	}
	
	public default void setCookie(final String key, final String value, final String path)
	{
		setCookie(key, value, path, null);
	}
	
	public void setCookie(
		final String key,
		final String value,
		final String path,
		final Duration lifespan);
	
	public default void deleteCookie(final String key)
	{
		setCookie(key, "");
	}
	
	public String getCookie(final String key);
	
	///////////////////////////////////////////////////////////////////////////
	// static methods//
	/////////////////////////////////////////////////
	
	public static Cookies getCurrent()
	{
		return VaadinSession.getCurrent().getAttribute(Cookies.class);
	}
	
	static void initFor(final VaadinSession session)
	{
		session.setAttribute(Cookies.class, new Default(session));
	}
	
	public static class Default implements Cookies
	{
		private Cookie[] cookies;
		
		protected Default(final VaadinSession vs)
		{
			vs.addRequestHandler((session, request, response) -> {
				
				this.cookies = request.getCookies();
				
				return false;
			});
		}
		
		@Override
		public void setCookie(
			final String key,
			final String value,
			final String path,
			final Duration lifespan)
		{
			final long   millis = lifespan != null ? lifespan.toMillis() : 0L;
			final String js     = String.format("var millis=%s;" + "var expires=\"\";\n"
				+ "if(millis>0){var date = new Date();date.setTime(date.getTime()+millis);"
				+ "expires=\";expires=\"+date.toGMTString();}\n"
				+ "document.cookie=\"%s=%s;path=%s\"+expires;", millis, key, value, path);
			UI.getCurrent().getPage().executeJs(js);
		}
		
		@Override
		public String getCookie(final String key)
		{
			if(this.cookies == null)
			{
				return null;
			}
			
			return Arrays.stream(this.cookies).filter(cookie -> cookie.getName().equals(key))
				.map(Cookie::getValue).findFirst().orElse(null);
		}
	}
}
