
package com.rapidclipse.framework.server.ui.filter;

import static java.util.Collections.unmodifiableCollection;
import static java.util.Objects.requireNonNull;

import java.util.Collection;


/**
 * @author XDEV Software
 *
 */
public interface FilterSubject
{
	public default FilterProperty<?> searchableProperty(final Object identifier)
	{
		return searchableProperties().stream()
			.filter(property -> property.identifier().equals(identifier)).findFirst()
			.orElse(null);
	}
	
	public Collection<FilterProperty<?>> searchableProperties();
	
	public default FilterProperty<?> filterableProperty(final Object identifier)
	{
		return filterableProperties().stream()
			.filter(property -> property.identifier().equals(identifier)).findFirst()
			.orElse(null);
	}
	
	public Collection<FilterProperty<?>> filterableProperties();
	
	public static FilterSubject New(
		final Collection<FilterProperty<?>> searchableProperties,
		final Collection<FilterProperty<?>> filterableProperties)
	{
		return new Implementation(searchableProperties, filterableProperties);
	}
	
	public static class Implementation implements FilterSubject
	{
		private final Collection<FilterProperty<?>> searchableProperties;
		private final Collection<FilterProperty<?>> filterableProperties;
		
		public Implementation(
			final Collection<FilterProperty<?>> searchableProperties,
			final Collection<FilterProperty<?>> filterableProperties)
		{
			super();
			
			this.searchableProperties = unmodifiableCollection(
				requireNonNull(searchableProperties));
			this.filterableProperties = unmodifiableCollection(
				requireNonNull(filterableProperties));
		}
		
		@Override
		public Collection<FilterProperty<?>> searchableProperties()
		{
			return this.searchableProperties;
		}
		
		@Override
		public Collection<FilterProperty<?>> filterableProperties()
		{
			return this.filterableProperties;
		}
	}
}
