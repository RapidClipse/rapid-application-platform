
package com.rapidclipse.framework.server.navigation;

import static java.util.Collections.unmodifiableMap;
import static java.util.Objects.requireNonNull;

import java.io.Serializable;
import java.util.Map;


/**
 * @author XDEV Software
 *
 */
public interface NavigationParameters extends Serializable
{
	public Iterable<String> names();
	
	public Object value(final String name);
	
	public static NavigationParameters New(final Map<String, Object> mapping)
	{
		return new Implementation(mapping);
	}
	
	public static class Implementation implements NavigationParameters
	{
		private final Map<String, Object> mapping;
		
		public Implementation(final Map<String, Object> mapping)
		{
			super();
			this.mapping = unmodifiableMap(requireNonNull(mapping));
		}
		
		@Override
		public Iterable<String> names()
		{
			return this.mapping.keySet();
		}
		
		@Override
		public Object value(final String name)
		{
			return this.mapping.get(name);
		}
	}
}
