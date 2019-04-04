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
		session.setAttribute(Cookies.class, new Implementation(session));
	}
	
	public static class Implementation implements Cookies
	{
		private Cookie[] cookies;
		
		public Implementation(final VaadinSession vs)
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
			UI.getCurrent().getPage().executeJavaScript(js);
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
