/*
 * Copyright (C) 2013-2023 by XDEV Software, All Rights Reserved.
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
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software - initial API and implementation
 */
package com.rapidclipse.framework.server.jpa.dal;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import javax.persistence.CacheRetrieveMode;
import javax.persistence.CacheStoreMode;
import javax.persistence.metamodel.Attribute;

import org.apache.commons.lang3.StringUtils;

import com.rapidclipse.framework.server.jpa.AttributeChain;


/**
 * The SearchParameters is used to pass search parameters to the DAO layer.
 * <p>
 * Its usage keeps 'find' method signatures in the DAO/Service layer simple.
 * <p>
 * A SearchParameters helps you drive your search in the following areas:
 * <ul>
 * <li>Configure the search mode (EQUALS, LIKE, ...)</li>
 * <li>Pagination: it allows you to limit your search results to a specific
 * range.</li>
 * <li>Allow you to specify ORDER BY and ASC/DESC</li>
 * <li>Enable/disable case sensitivity</li>
 * <li>Enable/disable 2d level cache</li>
 * <li>LIKE search against all string values: simply set the searchPattern
 * property</li>
 * <li>Named query: if you set a named query it will be executed. Named queries
 * can be defined in annotation or src/main/resources/META-INF/orm.xml</li>
 * <li>FullTextSearch: simply set the term property (requires Hibernate
 * Search)</li>
 * </ul>
 * <p>
 * Note : All requests are limited to a maximum number of elements to prevent
 * resource exhaustion.
 *
 * @see SearchMode
 * @see OrderBy
 * @see Range
 * @see PropertySelector
 *
 * @author XDEV Software
 */
public interface SearchParameters extends Serializable
{
	/**
	 * Set the @{link SearchMode}. It defaults to EQUALS.
	 *
	 * @param searchMode
	 *            searchmode
	 *
	 * @see SearchMode#EQUALS
	 */
	public void setSearchMode(final SearchMode searchMode);
	
	/**
	 * Return the @{link SearchMode}. It defaults to EQUALS.
	 *
	 * @see SearchMode#EQUALS
	 */
	public SearchMode getSearchMode();
	
	public default boolean is(final SearchMode searchMode)
	{
		return getSearchMode() == searchMode;
	}
	
	/**
	 * Fluently set the @{link SearchMode}. It defaults to EQUALS.
	 *
	 * @param searchMode
	 *            searchmode
	 *
	 * @see SearchMode#EQUALS
	 */
	public default SearchParameters searchMode(final SearchMode searchMode)
	{
		setSearchMode(searchMode);
		return this;
	}
	
	/**
	 * Use the EQUALS @{link SearchMode}.
	 *
	 * @param searchMode
	 *            searchmode
	 *
	 * @see SearchMode#EQUALS
	 */
	public default SearchParameters equals()
	{
		return searchMode(SearchMode.EQUALS);
	}
	
	/**
	 * Use the ANYWHERE @{link SearchMode}.
	 *
	 * @param searchMode
	 *            searchmode
	 *
	 * @see SearchMode#ANYWHERE
	 */
	public default SearchParameters anywhere()
	{
		return searchMode(SearchMode.ANYWHERE);
	}
	
	/**
	 * Use the STARTING_LIKE @{link SearchMode}.
	 *
	 * @see SearchMode#STARTING_LIKE
	 */
	public default SearchParameters startingLike()
	{
		return searchMode(SearchMode.STARTING_LIKE);
	}
	
	/**
	 * Use the LIKE @{link SearchMode}.
	 *
	 * @see SearchMode#LIKE
	 */
	public default SearchParameters like()
	{
		return searchMode(SearchMode.LIKE);
	}
	
	/**
	 * Use the ENDING_LIKE @{link SearchMode}.
	 *
	 * @see SearchMode#ENDING_LIKE
	 */
	public default SearchParameters endingLike()
	{
		return searchMode(SearchMode.ENDING_LIKE);
	}
	
	public void setAndMode(final boolean andMode);
	
	public boolean isAndMode();
	
	public default SearchParameters andMode()
	{
		setAndMode(true);
		return this;
	}
	
	public default SearchParameters orMode()
	{
		setAndMode(false);
		return this;
	}
	
	/**
	 * When it returns true, it indicates to the DAO layer to use the given
	 * searchPattern on all string properties.
	 */
	public boolean hasSearchPattern();
	
	/**
	 * Set the pattern which may contains wildcards (ex: <code>e%r%ka</code> ).
	 * <p>
	 * The given searchPattern is used by the DAO layer on all string
	 * properties. Null by default.
	 */
	public void setSearchPattern(final String searchPattern);
	
	public String getSearchPattern();
	
	public default SearchParameters searchPattern(final String searchPattern)
	{
		setSearchPattern(searchPattern);
		return this;
	}
	
	/**
	 * Set the case sensitiveness. Defaults to false.
	 *
	 * @param caseSensitive
	 *            caseSensitive
	 */
	public void setCaseSensitive(final boolean caseSensitive);
	
	public boolean isCaseSensitive();
	
	public default boolean isCaseInsensitive()
	{
		return !isCaseSensitive();
	}
	
	/**
	 * Fluently set the case sensitiveness. Defaults to false.
	 *
	 * @param caseSensitive
	 *            caseSensitive
	 */
	public default SearchParameters caseSensitive(final boolean caseSensitive)
	{
		setCaseSensitive(caseSensitive);
		return this;
	}
	
	/**
	 * Fluently set the case sensitiveness to true.
	 */
	public default SearchParameters caseSensitive()
	{
		return caseSensitive(true);
	}
	
	/**
	 * Fluently set the case sensitiveness to false.
	 */
	public default SearchParameters caseInsensitive()
	{
		return caseSensitive(false);
	}
	
	public void setPredicateSupplier(final PredicateSupplier predicateSupplier);
	
	public PredicateSupplier getPredicateSupplier();
	
	public default SearchParameters predicate(final PredicateSupplier predicateSupplier)
	{
		setPredicateSupplier(predicateSupplier);
		return this;
	}
	
	public Collection<OrderBy> getOrders();
	
	public void addOrderBy(final OrderBy orderBy);
	
	public default boolean hasOrders()
	{
		return !getOrders().isEmpty();
	}
	
	public default SearchParameters orderBy(final OrderBy... orderBys)
	{
		for(final OrderBy orderBy : orderBys)
		{
			addOrderBy(orderBy);
		}
		return this;
	}
	
	public default SearchParameters asc(final Attribute<?, ?>... attributes)
	{
		return orderBy(OrderBy.New(OrderBy.Direction.ASC, attributes));
	}
	
	public default SearchParameters desc(final Attribute<?, ?>... attributes)
	{
		return orderBy(OrderBy.New(OrderBy.Direction.DESC, attributes));
	}
	
	public default SearchParameters orderBy(
		final OrderBy.Direction orderByDirection,
		final Attribute<?, ?>... attributes)
	{
		return orderBy(OrderBy.New(orderByDirection, attributes));
	}
	
	public default SearchParameters asc(final String property, final Class<?> from)
	{
		return orderBy(OrderBy.New(OrderBy.Direction.ASC, property, from));
	}
	
	public default SearchParameters desc(final String property, final Class<?> from)
	{
		return orderBy(OrderBy.New(OrderBy.Direction.DESC, property, from));
	}
	
	public default SearchParameters orderBy(
		final OrderBy.Direction orderByDirection,
		final String property,
		final Class<?> from)
	{
		return orderBy(OrderBy.New(orderByDirection, property, from));
	}
	
	public Collection<Range<?, ?>> getRanges();
	
	public void addRange(final Range<?, ?> range);
	
	public default boolean hasRanges()
	{
		return !getRanges().isEmpty();
	}
	
	public default SearchParameters range(final Range<?, ?>... ranges)
	{
		for(final Range<?, ?> range : ranges)
		{
			addRange(range);
		}
		return this;
	}
	
	public default <D extends Comparable<? super D>> SearchParameters range(
		final D from,
		final D to,
		final Attribute<?, ?>... attributes)
	{
		return range(Range.New(from, to, attributes));
	}
	
	public Collection<PropertySelector<?, ?>> getProperties();
	
	public void addProperty(final PropertySelector<?, ?> propertySelector);
	
	public default boolean hasProperties()
	{
		return !getProperties().isEmpty();
	}
	
	public default SearchParameters property(final PropertySelector<?, ?>... propertySelectors)
	{
		for(final PropertySelector<?, ?> propertySelector : propertySelectors)
		{
			addProperty(propertySelector);
		}
		return this;
	}
	
	@SuppressWarnings("unchecked")
	public default <F> SearchParameters property(final Attribute<?, ?> fields, final F... selected)
	{
		return property(PropertySelector.New(fields).selected(selected));
	}
	
	/**
	 * Set the maximum number of results to retrieve. Pass -1 for no limits.
	 */
	public void setMaxResults(final int maxResults);
	
	public int getMaxResults();
	
	/**
	 * Set the position of the first result to retrieve.
	 *
	 * @param first
	 *            position of the first result, numbered from 0
	 */
	public void setFirst(final int first);
	
	public int getFirst();
	
	/**
	 * Set the page size, that is the maximum number of result to retrieve.
	 */
	public void setPageSize(final int pageSize);
	
	public int getPageSize();
	
	public default SearchParameters maxResults(final int maxResults)
	{
		setMaxResults(maxResults);
		return this;
	}
	
	public default SearchParameters noLimit()
	{
		setMaxResults(-1);
		return this;
	}
	
	public default SearchParameters first(final int first)
	{
		setFirst(first);
		return this;
	}
	
	public default SearchParameters pageSize(final int pageSize)
	{
		setPageSize(pageSize);
		return this;
	}
	
	/**
	 * Returns the attributes (x-to-one association) which must be fetched with
	 * a left join.
	 */
	public Collection<AttributeChain<?, ?>> getFetches();
	
	public default boolean hasFetches()
	{
		return !getFetches().isEmpty();
	}
	
	/**
	 * The given attribute (x-to-one association) will be fetched with a left
	 * join.
	 */
	public void addFetch(final Attribute<?, ?>... attributes);
	
	public void addFetch(final List<? extends Attribute<?, ?>> attributes);
	
	/**
	 * Fluently set the fetch attribute
	 */
	public default SearchParameters fetch(final Attribute<?, ?>... attributes)
	{
		addFetch(attributes);
		return this;
	}
	
	/**
	 * Fluently set the fetch attribute
	 */
	public default SearchParameters fetch(final List<? extends Attribute<?, ?>> attributes)
	{
		addFetch(attributes);
		return this;
	}
	
	/**
	 * Default to false. Please read
	 * https://hibernate.atlassian.net/browse/HHH-1523 before using cache.
	 */
	public void setCacheable(final boolean cacheable);
	
	public boolean isCacheable();
	
	public void setCacheRegion(final String cacheRegion);
	
	public String getCacheRegion();
	
	public default boolean hasCacheRegion()
	{
		return !StringUtils.isBlank(getCacheRegion());
	}
	
	public void setCacheStoreMode(final CacheStoreMode cacheStoreMode);
	
	public CacheStoreMode getCacheStoreMode();
	
	public default boolean hasCacheStoreMode()
	{
		return getCacheStoreMode() != null;
	}
	
	public void setCacheRetrieveMode(final CacheRetrieveMode cacheRetrieveMode);
	
	public CacheRetrieveMode getCacheRetrieveMode();
	
	public default boolean hasCacheRetrieveMode()
	{
		return getCacheRetrieveMode() != null;
	}
	
	public default SearchParameters cacheable(final boolean cacheable)
	{
		setCacheable(cacheable);
		return this;
	}
	
	public default SearchParameters enableCache()
	{
		setCacheable(true);
		return this;
	}
	
	public default SearchParameters disableCache()
	{
		setCacheable(false);
		return this;
	}
	
	public default SearchParameters cacheRegion(final String cacheRegion)
	{
		setCacheRegion(cacheRegion);
		return this;
	}
	
	public default SearchParameters cacheStoreMode(final CacheStoreMode cacheStoreMode)
	{
		setCacheStoreMode(cacheStoreMode);
		return this;
	}
	
	public default SearchParameters cacheRetrieveMode(final CacheRetrieveMode cacheRetrieveMode)
	{
		setCacheRetrieveMode(cacheRetrieveMode);
		return this;
	}
	
	public void setUseAndInXToMany(final boolean useAndInXToMany);
	
	public boolean getUseAndInXToMany();
	
	public default SearchParameters useOrInXToMany()
	{
		return useAndInXToMany(false);
	}
	
	public default SearchParameters useAndInXToMany()
	{
		return useAndInXToMany(true);
	}
	
	public default SearchParameters useAndInXToMany(final boolean xToManyAndMode)
	{
		setUseAndInXToMany(xToManyAndMode);
		return this;
	}
	
	public void setDistinct(final boolean useDistinct);
	
	public boolean getDistinct();
	
	public default SearchParameters distinct(final boolean useDistinct)
	{
		setDistinct(useDistinct);
		return this;
	}
	
	public default SearchParameters distinct()
	{
		return distinct(true);
	}
	
	public static SearchParameters New()
	{
		return new Default();
	}
	
	public static class Default implements SearchParameters
	{
		private SearchMode searchMode = SearchMode.EQUALS;
		private boolean    andMode    = true;
		
		private final Set<OrderBy> orders = new HashSet<>();
		
		private boolean caseSensitive = true;
		
		private PredicateSupplier predicateSupplier;
		
		// pagination
		private int maxResults = -1;
		private int first      = 0;
		private int pageSize   = 0;
		
		private final Set<PathHolder> fetches = new HashSet<>();
		
		private final List<Range<?, ?>> ranges = new ArrayList<>();
		
		private final List<PropertySelector<?, ?>> properties = new ArrayList<>();
		
		// pattern to match against all strings.
		private String searchPattern;
		
		// Warn: before enabling cache for queries,
		// check this: https://hibernate.atlassian.net/browse/HHH-1523
		private Boolean           cacheable = false;
		private String            cacheRegion;
		private CacheStoreMode    cacheStoreMode;
		private CacheRetrieveMode cacheRetrieveMode;
		
		private boolean useAndInXToMany = true;
		
		private boolean useDistinct = false;
		
		protected Default()
		{
			super();
		}
		
		@Override
		public void setSearchMode(final SearchMode searchMode)
		{
			this.searchMode = searchMode;
		}
		
		@Override
		public SearchMode getSearchMode()
		{
			return this.searchMode;
		}
		
		@Override
		public void setAndMode(final boolean andMode)
		{
			this.andMode = andMode;
		}
		
		@Override
		public boolean isAndMode()
		{
			return this.andMode;
		}
		
		@Override
		public boolean hasSearchPattern()
		{
			return this.searchPattern != null && this.searchPattern.trim().length() > 0;
		}
		
		@Override
		public void setSearchPattern(final String searchPattern)
		{
			this.searchPattern = searchPattern;
		}
		
		@Override
		public String getSearchPattern()
		{
			return this.searchPattern;
		}
		
		@Override
		public void setCaseSensitive(final boolean caseSensitive)
		{
			this.caseSensitive = caseSensitive;
		}
		
		@Override
		public boolean isCaseSensitive()
		{
			return this.caseSensitive;
		}
		
		@Override
		public void setPredicateSupplier(final PredicateSupplier predicateSupplier)
		{
			this.predicateSupplier = predicateSupplier;
		}
		
		@Override
		public PredicateSupplier getPredicateSupplier()
		{
			return this.predicateSupplier;
		}
		
		@Override
		public Collection<OrderBy> getOrders()
		{
			return this.orders;
		}
		
		@Override
		public void addOrderBy(final OrderBy orderBy)
		{
			if(!this.orders.add(orderBy))
			{
				throw new IllegalArgumentException("Duplicate orderBy: '" + orderBy + "'.");
			}
		}
		
		@Override
		public Collection<Range<?, ?>> getRanges()
		{
			return this.ranges;
		}
		
		@Override
		public void addRange(final Range<?, ?> range)
		{
			this.ranges.add(range);
		}
		
		@Override
		public Collection<PropertySelector<?, ?>> getProperties()
		{
			return this.properties;
		}
		
		@Override
		public void addProperty(final PropertySelector<?, ?> propertySelector)
		{
			this.properties.add(propertySelector);
		}
		
		@Override
		public void setMaxResults(final int maxResults)
		{
			this.maxResults = maxResults;
		}
		
		@Override
		public int getMaxResults()
		{
			return this.maxResults;
		}
		
		@Override
		public void setFirst(final int first)
		{
			this.first = first;
		}
		
		@Override
		public int getFirst()
		{
			return this.first;
		}
		
		@Override
		public void setPageSize(final int pageSize)
		{
			this.pageSize = pageSize;
		}
		
		@Override
		public int getPageSize()
		{
			return this.pageSize;
		}
		
		@Override
		public Collection<AttributeChain<?, ?>> getFetches()
		{
			return this.fetches.stream().map(PathHolder::getAttributes).collect(Collectors.toList());
		}
		
		@Override
		public void addFetch(final Attribute<?, ?>... attributes)
		{
			this.fetches.add(PathHolder.New(AttributeChain.New(attributes)));
		}
		
		@Override
		public void addFetch(final List<? extends Attribute<?, ?>> attributes)
		{
			this.fetches.add(PathHolder.New(AttributeChain.New(attributes)));
		}
		
		@Override
		public void setCacheable(final boolean cacheable)
		{
			this.cacheable = cacheable;
		}
		
		@Override
		public boolean isCacheable()
		{
			return this.cacheable;
		}
		
		@Override
		public void setCacheRegion(final String cacheRegion)
		{
			this.cacheRegion = cacheRegion;
		}
		
		@Override
		public String getCacheRegion()
		{
			return this.cacheRegion;
		}
		
		@Override
		public void setCacheStoreMode(final CacheStoreMode cacheStoreMode)
		{
			this.cacheStoreMode = cacheStoreMode;
		}
		
		@Override
		public CacheStoreMode getCacheStoreMode()
		{
			return this.cacheStoreMode;
		}
		
		@Override
		public void setCacheRetrieveMode(final CacheRetrieveMode cacheRetrieveMode)
		{
			this.cacheRetrieveMode = cacheRetrieveMode;
		}
		
		@Override
		public CacheRetrieveMode getCacheRetrieveMode()
		{
			return this.cacheRetrieveMode;
		}
		
		@Override
		public void setUseAndInXToMany(final boolean useAndInXToMany)
		{
			this.useAndInXToMany = useAndInXToMany;
		}
		
		@Override
		public boolean getUseAndInXToMany()
		{
			return this.useAndInXToMany;
		}
		
		@Override
		public void setDistinct(final boolean useDistinct)
		{
			this.useDistinct = useDistinct;
		}
		
		@Override
		public boolean getDistinct()
		{
			return this.useDistinct;
		}
	}
}
