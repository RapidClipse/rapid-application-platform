
package com.rapidclipse.framework.server.ui.filter;

import static java.util.Objects.requireNonNull;


public interface FilterProperty<T>
{
	public Object identifier();
	
	public Class<T> type();
	
	public String caption();
	
	public static <T> FilterProperty<T> New(
		final Object name,
		final Class<T> type,
		final String caption)
	{
		return new Implementation<>(name, type, caption);
	}
	
	public static class Implementation<T> implements FilterProperty<T>
	{
		private final Object   identifier;
		private final Class<T> type;
		private final String   caption;
		
		public Implementation(final Object identifier, final Class<T> type, final String caption)
		{
			super();
			
			this.identifier = requireNonNull(identifier);
			this.type       = requireNonNull(type);
			this.caption    = requireNonNull(caption);
		}
		
		@Override
		public Object identifier()
		{
			return this.identifier;
		}
		
		@Override
		public Class<T> type()
		{
			return this.type;
		}
		
		@Override
		public String caption()
		{
			return this.caption;
		}
	}
}
