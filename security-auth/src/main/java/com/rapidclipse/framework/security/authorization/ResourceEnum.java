
package com.rapidclipse.framework.security.authorization;

/**
 * Contract for central resource collection enums.
 *
 * @author XDEV Software
 */
public interface ResourceEnum<E extends Enum<E> & ResourceEnum<E>>
{
	/**
	 * The resource's name
	 *
	 * @return the name of the resource
	 */
	public String resourceName();
	
	/**
	 * Gets the actual resource held by this enum entry.
	 *
	 * @return the actual resource
	 */
	public Resource resource();
}
