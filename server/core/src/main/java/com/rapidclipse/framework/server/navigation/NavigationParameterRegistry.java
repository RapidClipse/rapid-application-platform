
package com.rapidclipse.framework.server.navigation;

import static com.rapidclipse.framework.server.Rap.sessionBoundInstance;
import static java.util.Objects.requireNonNull;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import java.util.function.Function;

import com.rapidclipse.framework.server.data.ValueTransfer;


/**
 * @author XDEV Software
 *
 */
public interface NavigationParameterRegistry extends Serializable
{
	public String put(final NavigationParameters parameters);
	
	public NavigationParameters get(final String id);
	
	public static NavigationParameterRegistry getCurrent()
	{
		return sessionBoundInstance(NavigationParameterRegistry.class, Implementation::new);
	}
	
	public static class Implementation implements NavigationParameterRegistry
	{
		private final Map<String, NavigationParameters> map = new HashMap<>();
		
		public Implementation()
		{
			super();
		}
		
		@Override
		public synchronized String put(final NavigationParameters parameters)
		{
			requireNonNull(parameters);
			
			final String id = getNewId();
			
			this.map.put(id, transform(parameters, ValueTransfer::put));
			
			return id;
		}
		
		@Override
		public synchronized NavigationParameters get(final String id)
		{
			final NavigationParameters parameters = this.map.get(id);
			if(parameters == null)
			{
				throw new NavigationException("Navigation state not found: " + id);
			}
			
			return transform(parameters, ValueTransfer::get);
		}
		
		protected String getNewId()
		{
			String id;
			do
			{
				id = UUID.randomUUID().toString();
			}
			while(this.map.containsKey(id));
			
			return id;
		}
		
		protected NavigationParameters transform(
			final NavigationParameters parameters,
			final Function<Object, Object> logic)
		{
			final Map<String, Object> transformed = new HashMap<>();
			
			for(final String name : parameters.names())
			{
				final Object value            = parameters.value(name);
				final Object transformedValue = logic.apply(value);
				transformed.put(name, transformedValue);
			}
			
			return NavigationParameters.New(transformed);
		}
	}
}
