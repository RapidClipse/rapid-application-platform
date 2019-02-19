
package com.rapidclipse.framework.server.ui.filter;

import java.io.Serializable;

import com.rapidclipse.framework.server.data.filter.Comparison.StringComparison;
import com.rapidclipse.framework.server.data.filter.Filter;
import com.vaadin.flow.function.SerializableFunction;


/**
 * @author XDEV Software
 *
 */
public final class FilterConverterFactory
{
	public static SerializableFunction<String, Filter> New(final Serializable identifier)
	{
		return New(identifier, false);
	}
	
	public static SerializableFunction<String, Filter> New(
		final Serializable identifier,
		final boolean caseSensitive)
	{
		return filter -> {
			
			String pattern;
			if(filter == null || (pattern = filter.trim()).isEmpty())
			{
				return null;
			}
			
			final int length = pattern.length();
			if(length > 0 && pattern.charAt(length - 1) != StringComparison.DEFAULT_WILDCARD)
			{
				pattern += StringComparison.DEFAULT_WILDCARD;
			}
			
			return Filter.StringComparison(identifier, pattern, caseSensitive);
		};
	}
	
	private FilterConverterFactory()
	{
		throw new Error();
	}
}
