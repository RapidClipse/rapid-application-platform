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


import static java.util.Objects.requireNonNull;


public interface FilterProperty
{
	public Object identifier();


	public Class<?> type();


	public String caption();


	public static FilterProperty New(final Object name, final Class<?> type, final String caption)
	{
		return new Implementation(name,type,caption);
	}



	public static class Implementation implements FilterProperty
	{
		private final Object	identifier;
		private final Class<?>	type;
		private final String	caption;


		public Implementation(final Object identifier, final Class<?> type, final String caption)
		{
			super();

			this.identifier = requireNonNull(identifier);
			this.type = requireNonNull(type);
			this.caption = requireNonNull(caption);
		}


		@Override
		public Object identifier()
		{
			return this.identifier;
		}


		@Override
		public Class<?> type()
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