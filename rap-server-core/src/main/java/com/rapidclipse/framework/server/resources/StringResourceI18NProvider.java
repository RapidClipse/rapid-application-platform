/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.resources;

import java.util.Arrays;
import java.util.List;
import java.util.Locale;
import java.util.MissingResourceException;

import com.vaadin.flow.i18n.I18NProvider;


/**
 * Bridge from Vaadin's {@link I18NProvider} to the {@link StringResourceProvider}.
 *
 * @author XDEV Software
 *
 */
public class StringResourceI18NProvider implements I18NProvider
{
	private final List<Locale> providedLocales = Arrays.asList(Locale.getAvailableLocales());
	
	public StringResourceI18NProvider()
	{
		super();
	}
	
	@Override
	public List<Locale> getProvidedLocales()
	{
		return this.providedLocales;
	}
	
	@Override
	public String getTranslation(final String key, final Locale locale, final Object... params)
	{
		try
		{
			return StringResourceUtils.getResourceString(key, locale, getCallerClass());
		}
		catch(final MissingResourceException e)
		{
			// I18NProvider contract: error string instead of exception
			return "!{" + key + "}!";
		}
	}
	
	private Class<?> getCallerClass()
	{
		for(final StackTraceElement elem : new Exception().getStackTrace())
		{
			final String className = elem.getClassName();
			if(!className.equals(StringResourceI18NProvider.class.getName()) &&
				!className.startsWith("com.vaadin."))
			{
				try
				{
					return Class.forName(className);
				}
				catch(final ClassNotFoundException e)
				{
					break;
				}
			}
		}
		return getClass();
	}
}
