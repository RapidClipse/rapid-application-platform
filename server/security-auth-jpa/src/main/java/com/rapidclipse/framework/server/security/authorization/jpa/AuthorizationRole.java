
package com.rapidclipse.framework.server.security.authorization.jpa;

import java.util.Collection;


/**
 *
 * @author XDEV Software (CK)
 */
public interface AuthorizationRole
{
	public String roleName();
	
	public Collection<? extends AuthorizationResource> resources();
	
	public Collection<? extends AuthorizationRole> roles();
}
