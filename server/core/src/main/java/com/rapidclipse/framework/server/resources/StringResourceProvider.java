/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
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

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Member;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.MissingResourceException;
import java.util.PropertyResourceBundle;
import java.util.ResourceBundle;

import com.vaadin.flow.component.UI;
import com.vaadin.flow.i18n.I18NProvider;
import com.vaadin.flow.server.StreamResourceWriter;
import com.vaadin.flow.server.VaadinService;
import com.vaadin.flow.server.VaadinSession;


/**
 * Provides {@link String} resources which can be used for internationalization.
 *
 * @author XDEV Software
 *
 */

public interface StringResourceProvider
{
	/**
	 * Searches for a resource entry according to <code>key</code>.<br>
	 * The <code>requestor</code> may be crucial how the strategy is looking for
	 * the ressource.<br>
	 * <br>
	 * If <code>requestor</code> is <code>null</code> only the default resource
	 * bundle is searched through.
	 *
	 * @param key
	 *            the key of the resource's value pair
	 * @param locale
	 *            to lookup the String for
	 * @param requestor
	 *            the origin of the call to this method or <code>null</code>
	 * @return the value mapped to <code>key</code>
	 * @throws MissingResourceException
	 *             if no resource bundle can be found - depending on this search
	 *             strategy - or if the key can not be found in one of the
	 *             resource files
	 * @throws NullPointerException
	 *             if <code>key</code> is <code>null</code>
	 */

	public String lookupResourceString(String key, Locale locale, Object requestor)
		throws MissingResourceException, NullPointerException;

	public static StringResourceProvider New()
	{
		return new Default();
	}

	public static class Default implements StringResourceProvider
	{
		protected final Map<Locale, ResourceBundle> localizedProjectBundles;
		protected final ResourceBundle              defaultProjectBundle;

		protected Default()
		{
			super();
			this.localizedProjectBundles = new HashMap<>();
			this.defaultProjectBundle    = loadProjectResourceBundle(null, this);
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public String lookupResourceString(final String key, Locale locale, final Object requestor)
			throws MissingResourceException
		{
			if(locale == null)
			{
				locale = getLocale();
			}

			Class<?> clazz = null;
			if(requestor != null)
			{
				if(requestor instanceof Class)
				{
					clazz = (Class<?>)requestor;
				}
				else if(requestor instanceof Member)
				{
					clazz = ((Member)requestor).getDeclaringClass();
				}
				else
				{
					clazz = requestor.getClass();
				}
			}

			if(clazz != null)
			{
				String  name  = clazz.getName();
				boolean first = true;

				while(true)
				{
					try
					{
						String baseName;
						if(first)
						{
							first    = false;
							baseName = name;
						}
						else
						{
							baseName = name.concat(".package");
						}

						return ResourceBundle.getBundle(baseName, locale, clazz.getClassLoader())
							.getString(key);
					}
					catch(final MissingResourceException mre)
					{
					}
					catch(final NullPointerException npe)
					{
					}

					final int lastDot = name.lastIndexOf('.');
					if(lastDot > 0)
					{
						name = name.substring(0, lastDot);
					}
					else
					{
						break;
					}
				}
			}

			ResourceBundle localizedProjectBundle = null;
			if(this.localizedProjectBundles.containsKey(locale))
			{
				localizedProjectBundle = this.localizedProjectBundles.get(locale);
			}
			else
			{
				localizedProjectBundle = loadProjectResourceBundle(locale, requestor);
				this.localizedProjectBundles.put(locale, localizedProjectBundle);
			}

			if(localizedProjectBundle != null)
			{
				try
				{
					return localizedProjectBundle.getString(key);
				}
				catch(final MissingResourceException e)
				{
				}
			}

			if(this.defaultProjectBundle != null)
			{
				try
				{
					return this.defaultProjectBundle.getString(key);
				}
				catch(final MissingResourceException e)
				{
				}
			}

			final String className = clazz != null ? clazz.getName() : getClass().getName();
			throw new MissingResourceException("No resource found for key '" + key
				+ "', requestor = " + className + ", locale = " + locale.getLanguage(),
				className, key);
		}

		protected Locale getLocale()
		{
			final UI currentUi = UI.getCurrent();
			Locale   locale    = currentUi == null ? null : currentUi.getLocale();
			if(locale == null)
			{
				final I18NProvider i18nProvider = VaadinService.getCurrent().getInstantiator()
					.getI18NProvider();
				final List<Locale> locales      = i18nProvider.getProvidedLocales();
				if(locales != null && !locales.isEmpty())
				{
					locale = locales.get(0);
				}
				else
				{
					locale = Locale.getDefault();
				}
			}
			return locale;
		}

		protected ResourceBundle loadProjectResourceBundle(
			final Locale locale,
			final Object requestor)
		{
			try
			{
				return ResourceBundle.getBundle("project");
			}
			catch(final MissingResourceException mre)
			{
				final String localeSuffix = locale != null ? "_" + locale.getLanguage() : "";

				try(InputStream in = getResource(
					getProjectBundlePath() + localeSuffix + ".properties", requestor))
				{
					if(in != null)
					{
						return new PropertyResourceBundle(in);
					}
				}
				catch(final IOException e)
				{
				}

				return null;
			}
		}

		protected String getProjectBundlePath()
		{
			return "WebContent/WEB-INF/resources/project";
		}

		protected InputStream getResource(final String path, final Object requestor)
			throws IOException
		{
			try
			{
				final StreamResourceWriter  writer = new ApplicationResource(requestor.getClass(),
					path).getWriter();
				final ByteArrayOutputStream out    = new ByteArrayOutputStream();
				writer.accept(out, VaadinSession.getCurrent());
				return new ByteArrayInputStream(out.toByteArray());
			}
			catch(final Exception e)
			{
				// not found
				return null;
			}
		}
	}
}
