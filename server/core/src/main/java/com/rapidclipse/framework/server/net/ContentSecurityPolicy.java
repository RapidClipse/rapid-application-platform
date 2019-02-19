
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
		
		public static class Implementation implements Builder
		{
			private final Map<String, Set<String>> directives = new LinkedHashMap<>();
			
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
		
		public static class Implementation implements Assembler
		{
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
		return new Builder.Implementation();
	}
	
	public static ContentSecurityPolicy New(final Map<String, Set<String>> directives)
	{
		return new Implementation(directives);
	}
	
	public static Assembler Assembler()
	{
		return new Assembler.Implementation();
	}
	
	public static class Implementation implements ContentSecurityPolicy
	{
		private final Map<String, Set<String>> directives;
		
		public Implementation(final Map<String, Set<String>> directives)
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
