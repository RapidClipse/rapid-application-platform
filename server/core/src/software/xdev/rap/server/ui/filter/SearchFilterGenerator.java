/*
 * Copyright (C) 2013-2018 by XDEV Software, All Rights Reserved.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 *
 * For further information see
 * <http://www.rapidclipse.com/en/legal/license/license.html>.
 */

package software.xdev.rap.server.ui.filter;


import java.util.Arrays;

import software.xdev.rap.server.data.filter.Composite;
import software.xdev.rap.server.data.filter.Filter;


/**
 * @author XDEV Software
 *
 */
public interface SearchFilterGenerator
{
	public Filter createSearchFilter(String searchText, FilterContext context);
	
	
	public static SearchFilterGenerator New()
	{
		return new Implementation();
	}
	
	
	
	public static class Implementation implements SearchFilterGenerator
	{
		@Override
		public Filter createSearchFilter(String searchText, final FilterContext settings)
		{
			if(searchText == null || (searchText = searchText.trim()).length() == 0)
			{
				return null;
			}
			
			final String[] words = searchText.split(" ");
			
			if(words.length == 1)
			{
				return createSingleWordSearchFilter(words[0],settings);
			}
			
			return createMultiWordSearchFilter(words,settings);
		}
		
		
		protected Filter createSingleWordSearchFilter(final String word,
				final FilterContext context)
		{
			final Filter[] propertyFilters = context.getFilterSubject().searchableProperties()
					.stream()
					.map(searchableProperty -> createWordFilter(searchableProperty,word,context))
					.toArray(Filter[]::new);
			
			return combinePropertyFilters(propertyFilters,context);
		}
		
		
		protected Filter createMultiWordSearchFilter(final String[] words,
				final FilterContext context)
		{
			final Filter[] propertyFilters = context.getFilterSubject().searchableProperties()
					.stream()
					.map(searchableProperty -> Composite.New(context.getSearchMultiWordConnector(),
							Arrays.stream(words)
									.map(word -> createWordFilter(searchableProperty,word,context))
									.toArray(Filter[]::new)))
					.toArray(Filter[]::new);
			
			return combinePropertyFilters(propertyFilters,context);
		}
		
		
		protected Filter createWordFilter(final FilterProperty searchableProperty,
				final String word, final FilterContext context)
		{
			String pattern = word;
			final char wildcard = context.getWildcard();
			if(word.length() > 0 && word.charAt(word.length() - 1) != wildcard)
			{
				pattern += wildcard;
			}
			return Filter.StringComparison(searchableProperty.identifier(),pattern,
					context.isCaseSensitive(),Arrays.asList(wildcard));
		}
		
		
		protected Filter combinePropertyFilters(final Filter[] propertyFilters,
				final FilterContext settings)
		{
			switch(propertyFilters.length)
			{
				case 0:
					return null;
				
				case 1:
					return propertyFilters[0];
				
				default:
					return Composite.New(settings.getSearchPropertiesConnector(),propertyFilters);
			}
		}
	}
}
