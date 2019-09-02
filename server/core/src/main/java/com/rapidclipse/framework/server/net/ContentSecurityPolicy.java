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
