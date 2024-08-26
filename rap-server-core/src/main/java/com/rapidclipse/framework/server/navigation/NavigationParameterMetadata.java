/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
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

	public static NavigationParameterMetadata New(
		final Member member,
		final Class<?> type,
		final String name,
		final boolean optional)
	{
		return new Default(member, type, name, optional);
	}

	public static class Default implements NavigationParameterMetadata
	{
		private final Member   member;
		private final Class<?> type;
		private final String   name;
		private final boolean  optional;

		protected Default(
			final Member member,
			final Class<?> type,
			final String name,
			final boolean optional)
		{
			super();
			this.member   = requireNonNull(member);
			this.type     = requireNonNull(type);
			this.name     = requireNonNull(name);
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
