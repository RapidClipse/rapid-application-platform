/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
 *
 * This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * which accompanies this distribution, and is available at
 * https://www.eclipse.org/legal/epl-2.0/
 *
 * SPDX-License-Identifier: EPL-2.0
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
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
		return new Implementation(member, type, name, optional);
	}
	
	public static class Implementation implements NavigationParameterMetadata
	{
		private final Member   member;
		private final Class<?> type;
		private final String   name;
		private final boolean  optional;
		
		public Implementation(
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
