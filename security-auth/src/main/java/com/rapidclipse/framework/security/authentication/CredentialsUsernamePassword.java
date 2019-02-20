
package com.rapidclipse.framework.security.authentication;

import static java.util.Objects.requireNonNull;

import java.nio.charset.StandardCharsets;


/**
 * Trivial username / password string value tuple.
 *
 * @author XDEV Software (TM)
 */
public interface CredentialsUsernamePassword
{
	/**
	 * @return the username.
	 */
	public String username();
	
	/**
	 * @return the password.
	 */
	public byte[] password();
	
	/**
	 * Wraps the passed username and password strings as a new
	 * {@link CredentialsUsernamePassword} instance.
	 *
	 * @param username
	 *            the username to be wrapped.
	 * @param password
	 *            the password to be wrapped.
	 * @return a new {@link CredentialsUsernamePassword} instance containing the
	 *         passed credential values.
	 */
	public static CredentialsUsernamePassword.Implementation New(
		final String username,
		final byte[] password)
	{
		return new CredentialsUsernamePassword.Implementation(requireNonNull(username), requireNonNull(password));
	}
	
	/**
	 * Wraps the passed username and password strings as a new
	 * {@link CredentialsUsernamePassword} instance.
	 *
	 * @param username
	 *            the username to be wrapped.
	 * @param password
	 *            the password to be wrapped.
	 * @return a new {@link CredentialsUsernamePassword} instance containing the
	 *         passed credential values.
	 */
	public static CredentialsUsernamePassword.ImplementationString New(
		final String username,
		final String password)
	{
		return new CredentialsUsernamePassword.ImplementationString(requireNonNull(username),
			requireNonNull(password));
	}
	
	public final class ImplementationString implements CredentialsUsernamePassword
	{
		///////////////////////////////////////////////////////////////////////////
		// instance fields //
		////////////////////
		
		final String username;
		final String password;
		
		///////////////////////////////////////////////////////////////////////////
		// constructors //
		/////////////////
		
		ImplementationString(final String username, final String password)
		{
			super();
			this.username = username;
			this.password = password;
		}
		
		///////////////////////////////////////////////////////////////////////////
		// override methods //
		/////////////////////
		
		@Override
		public String username()
		{
			return this.username;
		}
		
		@Override
		public byte[] password()
		{
			return this.password.getBytes(StandardCharsets.UTF_8);
		}
		
		@Override
		public String toString()
		{
			// intentionally don't give away the actual password.
			return this.username + " // (PWD length " + this.password.length() + ")";
		}
	}
	
	public final class Implementation implements CredentialsUsernamePassword
	{
		///////////////////////////////////////////////////////////////////////////
		// instance fields //
		////////////////////
		
		final String username;
		final byte[] password;
		
		///////////////////////////////////////////////////////////////////////////
		// constructors //
		/////////////////
		
		Implementation(final String username, final byte[] password)
		{
			super();
			this.username = username;
			this.password = password;
		}
		
		///////////////////////////////////////////////////////////////////////////
		// override methods //
		/////////////////////
		
		@Override
		public String username()
		{
			return this.username;
		}
		
		@Override
		public byte[] password()
		{
			return this.password;
		}
		
		@Override
		public String toString()
		{
			// intentionally don't give away the actual password.
			return this.username + " // (PWD length " + this.password.length + ")";
		}
	}
}
