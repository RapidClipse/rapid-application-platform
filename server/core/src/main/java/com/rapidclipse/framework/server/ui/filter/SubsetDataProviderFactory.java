
package com.rapidclipse.framework.server.ui.filter;

import java.util.function.BiPredicate;
import java.util.function.Predicate;


/**
 * @author XDEV Software
 *
 */
@FunctionalInterface
public interface SubsetDataProviderFactory
{
	public SubsetDataProvider<?> createFor(
		final FilterContext context,
		final FilterProperty<?> property);
	
	public static <T> SubsetDataProviderFactory New(
		final Class<T> type,
		final SubsetDataProvider<T> provider)
	{
		return New(property -> type.equals(property.type()), provider);
	}
	
	public static SubsetDataProviderFactory New(
		final BiPredicate<FilterContext, FilterProperty<?>> predicate,
		final SubsetDataProvider<?> provider)
	{
		return (context, property) -> predicate.test(context, property) ? provider : null;
	}
	
	public static SubsetDataProviderFactory New(
		final Predicate<FilterProperty<?>> predicate,
		final SubsetDataProvider<?> provider)
	{
		return (context, property) -> predicate.test(property) ? provider : null;
	}
}
