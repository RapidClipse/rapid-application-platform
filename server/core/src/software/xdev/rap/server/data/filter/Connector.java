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
public interface Connector extends Filter
{
	public List<Filter> filters();



	public static abstract class Abstract implements Connector
	{
		private final List<Filter> filters;


		@SafeVarargs
		public Abstract(final Filter... filters)
		{
			super();

			this.filters = Arrays.asList(filters);
		}
		
		
		public Abstract(final List<Filter> filters)
		{
			super();
			
			this.filters = Collections.unmodifiableList(filters);
		}


		@Override
		public List<Filter> filters()
		{
			return this.filters;
		}
	}



	public static interface And extends Connector
	{
		public static class Implementation extends Abstract implements And
		{
			@SafeVarargs
			public Implementation(final Filter... filters)
			{
				super(filters);
			}


			public Implementation(final List<Filter> filters)
			{
				super(filters);
			}
		}
	}



	public static interface Or extends Connector
	{
		public static class Implementation extends Abstract implements Or
		{
			@SafeVarargs
			public Implementation(final Filter... filters)
			{
				super(filters);
			}


			public Implementation(final List<Filter> filters)
			{
				super(filters);
			}
		}
	}
}
