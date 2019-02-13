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

package com.rapidclipse.framework.server.navigation;


import static java.util.Objects.requireNonNull;

import java.lang.reflect.Member;


/**
 * @author XDEV Software
 *
 */
public interface NavigationParameterMetadata
{
	public Member member();


	public Class<?> type();
	
	
	public String name();
	
	
	public boolean optional();
	
	
	public static NavigationParameterMetadata New(final Member member, final Class<?> type,
			final String name, final boolean optional)
	{
		return new Implementation(member,type,name,optional);
	}
	
	
	
	public static class Implementation implements NavigationParameterMetadata
	{
		private final Member	member;
		private final Class<?>	type;
		private final String	name;
		private final boolean	optional;


		public Implementation(final Member member, final Class<?> type, final String name,
				final boolean optional)
		{
			super();
			this.member = requireNonNull(member);
			this.type = requireNonNull(type);
			this.name = requireNonNull(name);
			this.optional = optional;
		}


		@Override
		public Member member()
		{
			return this.member;
		}


		@Override
		public Class<?> type()
		{
			return this.type;
		}


		@Override
		public String name()
		{
			return this.name;
		}


		@Override
		public boolean optional()
		{
			return this.optional;
		}
	}
}
