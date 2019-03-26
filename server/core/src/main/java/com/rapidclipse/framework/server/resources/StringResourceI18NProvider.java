
package com.rapidclipse.framework.server.resources;

import java.util.Arrays;
import java.util.List;
import java.util.Locale;

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
		return StringResourceUtils.getResourceString(key, locale, getCallerClass());
	}
	
	private Class<?> getCallerClass()
	{
		for(final StackTraceElement elem : new Exception().getStackTrace())
		{
			final String className = elem.getClassName();
			if(!className.startsWith("com.vaadin.") &&
				!className.equals(StringResourceI18NProvider.class.getName()))
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
