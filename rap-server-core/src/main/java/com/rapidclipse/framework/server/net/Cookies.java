/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.net;

import java.io.Serializable;
import java.time.Duration;
import java.util.Arrays;

import jakarta.servlet.http.Cookie;

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
