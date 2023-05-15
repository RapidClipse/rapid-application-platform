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
package com.rapidclipse.framework.server.net;

import java.io.IOException;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;


/**
 * @author XDEV Software
 *
 */
public interface ContentSecurityPolicy
{
	public static interface Builder
	{
		public default Builder add(final ContentSecurityPolicy csp)
		{
			csp.keys().forEach(key -> add(key, csp.directives(key)));

			return this;
		}

		public Builder add(String key, String... values);

		public Builder add(final String key, final Iterable<String> values);

		public ContentSecurityPolicy build();

		public static class Default implements Builder
		{
			private final Map<String, Set<String>> directives = new LinkedHashMap<>();

			protected Default()
			{
				super();
			}

			@Override
			public Builder add(final String key, final Iterable<String> values)
			{
				final Set<String> set = this.directives.computeIfAbsent(key,
					k -> new LinkedHashSet<>());
				values.forEach(set::add);

				return this;
			}

			@Override
			public Builder add(final String key, final String... values)
			{
				final Set<String> set = this.directives.computeIfAbsent(key,
					k -> new LinkedHashSet<>());
				Arrays.stream(values).forEach(set::add);

				return this;
			}

			@Override
			public ContentSecurityPolicy build()
			{
				return New(this.directives);
			}
		}
	}

	public static interface Assembler
	{
		public default String assemble(final ContentSecurityPolicy csp)
		{
			return assemble(csp, new StringBuilder()).toString();
		}

		public Appendable assemble(ContentSecurityPolicy csp, Appendable appendable);

		public static class Default implements Assembler
		{
			protected Default()
			{
				super();
			}
			
			@Override
			public Appendable assemble(final ContentSecurityPolicy csp, final Appendable appendable)
			{
				final String string = csp.keys().stream().map(key -> {
					final Set<String> value = csp.directives(key);
					return key.concat(value.stream().collect(Collectors.joining(" ", " ", "")));
				}).collect(Collectors.joining("; "));

				try
				{
					appendable.append(string);
				}
				catch(final IOException e)
				{
					throw new RuntimeException(e);
				}

				return appendable;
			}
		}
	}

	public Set<String> keys();

	public Set<String> directives(final String key);

	public boolean isEmpty();

	public static Builder Builder()
	{
		return new Builder.Default();
	}

	public static ContentSecurityPolicy New(final Map<String, Set<String>> directives)
	{
		return new Default(directives);
	}

	public static Assembler Assembler()
	{
		return new Assembler.Default();
	}

	public static class Default implements ContentSecurityPolicy
	{
		private final Map<String, Set<String>> directives;

		protected Default(final Map<String, Set<String>> directives)
		{
			super();
			this.directives = Collections.unmodifiableMap(directives);
		}

		@Override
		public Set<String> keys()
		{
			return this.directives.keySet();
		}

		@Override
		public Set<String> directives(final String key)
		{
			return this.directives.get(key);
		}

		@Override
		public boolean isEmpty()
		{
			return this.directives.isEmpty()
				|| this.directives.values().stream().mapToInt(Set::size).sum() == 0;
		}

		@Override
		public String toString()
		{
			return ContentSecurityPolicy.Assembler().assemble(this);
		}
	}
}
