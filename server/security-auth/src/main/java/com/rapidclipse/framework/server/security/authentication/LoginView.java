
package com.rapidclipse.framework.server.security.authentication;

/**
 * A login view is used for authentication in an application.
 *
 * @author XDEV Software
 *
 */
public interface LoginView extends AccessibleView
{
	/**
	 * Returns the username of the login form.
	 *
	 * @return the username
	 */
	public String getUsername();
	
	/**
	 * Returns the password of the login form.
	 *
	 * @return the password
	 */
	public String getPassword();
}
