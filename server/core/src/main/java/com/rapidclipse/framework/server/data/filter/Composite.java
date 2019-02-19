
package com.rapidclipse.framework.server.data.filter;

import static com.rapidclipse.framework.server.Rap.notEmpty;
import static java.util.Arrays.asList;
import static java.util.Collections.unmodifiableList;

import java.util.List;


/**
 * @author XDEV Software
 *
 */
public interface Composite extends Filter
{
	public static enum Connector
	{
		AND,
		OR
	}
	
	public Connector connector();
	
	public List<Filter> filters();
	
	public static Composite New(final Connector connector, final Filter... filters)
	{
		return New(connector, asList(filters));
	}
	
	public static Composite New(final Connector connector, final List<Filter> filters)
	{
		return new Implementation(connector, filters);
	}
	
	public static class Implementation implements Composite
	{
		private final Connector    connector;
		private final List<Filter> filters;
		
		public Implementation(final Connector connector, final List<Filter> filters)
		{
			super();

			this.connector = connector;
			this.filters   = unmodifiableList(notEmpty(filters));
		}
		
		@Override
		public Connector connector()
		{
			return this.connector;
		}
		
		@Override
		public List<Filter> filters()
		{
			return this.filters;
		}
	}
}
