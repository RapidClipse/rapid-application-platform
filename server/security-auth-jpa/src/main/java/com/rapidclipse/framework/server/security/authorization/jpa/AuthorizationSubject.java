
package com.rapidclipse.framework.server.security.authorization.jpa;

import java.util.Collection;


/**
 *
 * @author XDEV Software (CK)
 */
public interface AuthorizationSubject
{
	public String subjectName();
	
	public Collection<? extends AuthorizationRole> roles();
}
