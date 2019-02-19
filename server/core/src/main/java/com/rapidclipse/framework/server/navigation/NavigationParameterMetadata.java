
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
