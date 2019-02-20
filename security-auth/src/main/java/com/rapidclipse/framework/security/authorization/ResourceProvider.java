
package com.rapidclipse.framework.security.authorization;

import java.util.Set;


/**
 * Function type that provides {@link Resource} instances.
 * For details, see {@link #provideResource(Resource, String, Set)}.
 *
 * @author XDEV Software (TM)
 */
@FunctionalInterface
public interface ResourceProvider
{
	/**
	 * Provides a suitable {@link Resource} instance based on the passed resource name, a potentially already
	 * existing instance and the collection of names of children (possibly empty).
	 * Providing means either validating an already existing instance or creating a fitting new instance.
	 *
	 * @param existingInstance
	 *            the potentially already existing {@link Resource} instance for the passed name.
	 * @param factor
	 *            the factor of the access to the passed {@link Resource} instance.
	 *
	 * @return a new {@link Resource} instance that satisfies the specified values.
	 */
	public Resource provideResource(Resource existingInstance, String name, Set<String> children);
}
