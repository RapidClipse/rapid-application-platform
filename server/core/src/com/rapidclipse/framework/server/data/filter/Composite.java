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
		AND, OR
	}


	public Connector connector();


	public List<Filter> filters();
	
	
	public static Composite New(final Connector connector, final Filter... filters)
	{
		return New(connector,asList(filters));
	}
	
	
	public static Composite New(final Connector connector, final List<Filter> filters)
	{
		return new Implementation(connector,filters);
	}
	
	
	
	public static class Implementation implements Composite
	{
		private final Connector		connector;
		private final List<Filter>	filters;


		public Implementation(final Connector connector, final List<Filter> filters)
		{
			super();
			
			this.connector = connector;
			this.filters = unmodifiableList(notEmpty(filters));
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
