/*
 * Copyright (C) 2013-2020 by XDEV Software, All Rights Reserved.
 *
 * This file is part of the RapidClipse Application Platform (RAP).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 */
package com.rapidclipse.framework.security;

import static java.util.Objects.requireNonNull;

import java.io.File;
import java.util.Map;
import java.util.function.BiConsumer;
import java.util.function.Consumer;

import com.rapidclipse.framework.security.authentication.AuthenticationFailedException;
import com.rapidclipse.framework.security.authentication.Authenticator;
import com.rapidclipse.framework.security.authentication.CredentialsUsernamePassword;
import com.rapidclipse.framework.security.authentication.InMemoryAuthenticator;
import com.rapidclipse.framework.security.authentication.InMemoryAuthenticatorProvider;
import com.rapidclipse.framework.security.authorization.AuthorizationManager;
import com.rapidclipse.framework.security.authorization.AuthorizationRegistry;
import com.rapidclipse.framework.security.authorization.Permission;
import com.rapidclipse.framework.security.authorization.Resource;
import com.rapidclipse.framework.security.authorization.Role;
import com.rapidclipse.framework.security.authorization.Subject;


/**
 * More specific {@link SecurityManager} type that defines {@link Authenticator} type parameters to
 * {@link CredentialsUsernamePassword} as the credentials type and {@link Boolean} as the result type for easier
 * usage of simple use cases.
 *
 * @author XDEV Software (TM)
 */
public interface InMemorySecurityManager extends SecurityManager<CredentialsUsernamePassword, Boolean>
{
	/**
	 * A specialized version of {@link #authenticate(CredentialsUsernamePassword)} that wraps the passed
	 * username and password in a new {@link CredentialsUsernamePassword} instane.
	 *
	 * @param username
	 *            the username to be used in the credentials.
	 * @param password
	 *            the password to be used in the credentials.
	 * @return the relayed return value of {@link #authenticate(CredentialsUsernamePassword)}.
	 * @throws AuthenticationFailedException
	 *             relays from {@link #authenticate(CredentialsUsernamePassword)}
	 */
	public default boolean authenticate(final String username, final String password)
		throws AuthenticationFailedException
	{
		return this.authenticate(CredentialsUsernamePassword.New(username, password));
	}

	/**
	 * A variation of {@link #login(String, String, Consumer, Consumer, BiConsumer)} that simply throws along any
	 * encountered exception
	 *
	 * @param username
	 *            the username to be used in the login process for authentication and {@link Subject} lookup.
	 * @param password
	 *            the password to be used in the login process for authentication.
	 * @param successAction
	 *            the action to be executed upon a successful authentication.
	 * @param failAction
	 *            the action to be executed upon a unsuccessful authentication.
	 */
	public default void login(
		final String username,
		final String password,
		final Consumer<? super Subject> successAction,
		final Consumer<? super String> failAction)
	{
		this.login(username, password, successAction, failAction, (u, e) -> {
			throw e;
		});
	}

	/**
	 * Specialized method for abstracting a typical login process:<br>
	 * If the passed username and password are successfully authenticated, the {@link Subject} instance identified
	 * by the username is determined and given to the passed <tt>successAction</tt> {@link Consumer}. Otherwise, the
	 * username is given to the passed <tt>failAction</tt> {@link Consumer}.<br>
	 * If any {@link RuntimeException} occurs during either of these actions, it is relayed to the passed
	 * <tt>exceptionHandler</tt> {@link BiConsumer} along with the username whose processing caused the exception.
	 * <p>
	 * In the simplest case, the passed exception handler can just pass the exception along.
	 * Also see {@link #login(String, String, Consumer, Consumer)}.
	 *
	 * @param username
	 *            the username to be used in the login process for authentication and {@link Subject} lookup.
	 * @param password
	 *            the password to be used in the login process for authentication.
	 * @param successAction
	 *            the action to be executed upon a successful authentication.
	 * @param failAction
	 *            the action to be executed upon a unsuccessful authentication.
	 * @param exceptionHandler
	 *            the callback logic to handle any occuring {@link RuntimeException} exception.
	 * @see #login(String, String, Consumer, Consumer)
	 */
	public default void login(
		final String username,
		final String password,
		final Consumer<? super Subject> successAction,
		final Consumer<? super String> failAction,
		final BiConsumer<? super String, ? super RuntimeException> exceptionHandler)
	{
		try
		{
			if(this.authenticate(username, password))
			{
				successAction.accept(this.subject(username));
			}
			else
			{
				failAction.accept(username);
			}
		}
		catch(final RuntimeException e)
		{
			exceptionHandler.accept(username, e);
			return;
		}
	}

	/**
	 * Creates a new {@link InMemorySecurityManager} instance using the passed {@link Authenticator} and
	 * {@link AuthorizationManager} instances as its internal delegates.
	 *
	 * @param authenticator
	 *            the authenticator to be used.
	 * @param authorizationManager
	 *            the authorizationManager to be used.
	 * @return a new {@link InMemorySecurityManager} instance
	 */
	public static InMemorySecurityManager New(
		final Authenticator<CredentialsUsernamePassword, Boolean> authenticator,
		final AuthorizationManager authorizationManager)
	{
		return new Default(
			requireNonNull(authenticator),
			requireNonNull(authorizationManager));
	}

	/**
	 * Creates a new {@link InMemorySecurityManager} instance by using default implementations to
	 * load the required configurations from the passed xml {@link File}.
	 *
	 * @param xmlFile
	 *            the file assumed to contain an XML content valid for default processing.
	 * @return a new {@link InMemorySecurityManager} instance
	 * @see InMemoryAuthenticatorProvider#provideAuthenticatorFromFile(File)
	 * @see AuthorizationManager#NewFromXmlFile(File)
	 */
	public static InMemorySecurityManager NewFromXml(final File xmlFile)
	{
		final InMemoryAuthenticator authenticator        =
			InMemoryAuthenticatorProvider.provideAuthenticatorFromFile(xmlFile);
		final AuthorizationManager  authorizationManager = AuthorizationManager.NewFromXmlFile(xmlFile);

		return New(authenticator, authorizationManager);
	}

	/**
	 * Default {@link InMemorySecurityManager} implementation that wraps delegate {@link Authenticator} and
	 * {@link AuthorizationManager} instances.
	 *
	 * @author XDEV Software (TM)
	 */
	public final class Default implements InMemorySecurityManager
	{
		///////////////////////////////////////////////////////////////////////////
		// instance fields //
		////////////////////

		private final Authenticator<CredentialsUsernamePassword, Boolean> authenticator;
		private final AuthorizationManager                                authorizationManager;

		///////////////////////////////////////////////////////////////////////////
		// constructors //
		/////////////////

		/**
		 * Implementation detail constructor that might change in the future.
		 */
		protected Default(
			final Authenticator<CredentialsUsernamePassword, Boolean> authenticator,
			final AuthorizationManager authorizationManager

		)
		{
			super();
			this.authenticator        = authenticator;
			this.authorizationManager = authorizationManager;
		}

		///////////////////////////////////////////////////////////////////////////
		// override methods //
		/////////////////////

		/**
		 * {@inheritDoc}
		 */
		@Override
		public final Boolean authenticate(final CredentialsUsernamePassword credentials)
			throws AuthenticationFailedException
		{
			return this.authenticator.authenticate(credentials);
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public final Permission providePermission(final Resource resource, final Integer factor)
		{
			return this.authorizationManager.providePermission(resource, factor);
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public final Map<String, Role> roles()
		{
			return this.authorizationManager.roles();
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public final Map<String, Subject> subjects()
		{
			return this.authorizationManager.subjects();
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public final Permission providePermission(final Resource resource)
		{
			return this.authorizationManager.providePermission(resource);
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public final Permission permission(final Resource resource, final Integer factor)
		{
			return this.authorizationManager.permission(resource, factor);
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public final Role role(final String roleName)
		{
			return this.authorizationManager.role(roleName);
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public final Subject subject(final String subjectName)
		{
			return this.authorizationManager.subject(subjectName);
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public final Object lockPermissionRegistry()
		{
			return this.authorizationManager.lockPermissionRegistry();
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public final Object lockRoleRegistry()
		{
			return this.authorizationManager.lockRoleRegistry();
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public final Object lockSubjectRegistry()
		{
			return this.authorizationManager.lockSubjectRegistry();
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public final Permission permission(final Resource resource)
		{
			return this.authorizationManager.permission(resource);
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public final AuthorizationRegistry authorizationRegistry()
		{
			return this.authorizationManager.authorizationRegistry();
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public final void reloadAuthorizations()
		{
			this.authorizationManager.reloadAuthorizations();
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public final Resource resource(final String name)
		{
			return this.authorizationManager.resource(name);
		}

	}

}
