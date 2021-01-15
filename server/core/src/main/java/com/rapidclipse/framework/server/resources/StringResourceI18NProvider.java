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
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
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
