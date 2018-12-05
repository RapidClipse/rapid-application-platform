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

package software.xdev.rap.server.data.filter;


import java.util.Arrays;
import java.util.Collections;
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
	
	
	
	public static class Implementation implements Composite
	{
		private final Connector		connector;
		private final List<Filter>	filters;
		
		
		@SafeVarargs
		public Implementation(final Connector connector, final Filter... filters)
		{
			super();
			
			this.connector = connector;
			this.filters = Arrays.asList(filters);
		}


		public Implementation(final Connector connector, final List<Filter> filters)
		{
			super();
			
			this.connector = connector;
			this.filters = Collections.unmodifiableList(filters);
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