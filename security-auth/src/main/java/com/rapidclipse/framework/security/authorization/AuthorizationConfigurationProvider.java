
package com.rapidclipse.framework.security.authorization;

/**
 * Function type that provides {@link AuthorizationConfiguration} instances.
 *
 * @author XDEV Software (TM)
 */
@FunctionalInterface
public interface AuthorizationConfigurationProvider
{
	/**
	 * Provides an authorization configuration in generic form from one or more sources.
	 *
	 * @return an authorization configuration instance.
	 */
	public AuthorizationConfiguration provideConfiguration();
}
