/*
 * Copyright (C) 2013-2018 by XDEV Software, All Rights Reserved.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 *
 * For further information see
 * <http://www.rapidclipse.com/en/legal/license/license.html>.
 */

package software.xdev.rap.security;

import static java.util.Objects.requireNonNull;

import java.util.Map;

import software.xdev.rap.security.authentication.AuthenticationFailedException;
import software.xdev.rap.security.authentication.Authenticator;
import software.xdev.rap.security.authorization.AuthorizationManager;
import software.xdev.rap.security.authorization.AuthorizationRegistry;
import software.xdev.rap.security.authorization.Permission;
import software.xdev.rap.security.authorization.Resource;
import software.xdev.rap.security.authorization.Role;
import software.xdev.rap.security.authorization.Subject;

/**
 * Security managing type that combines {@link Authenticator} and {@link AuthorizationManager} aspects.
 *
 * @param <C> the type of the credentials instance to be authenticated.
 * @param <R> the type of the result/response instance to be returned upon an authentication attempt.
 *
 * @author XDEV Software (TM)
 */
public interface SecurityManager<C, R> extends Authenticator<C, R>, AuthorizationManager
{
	public static <C, R> SecurityManager<C, R> New(
		final Authenticator<C, R>  authenticator       ,
		final AuthorizationManager authorizationManager
	)
	{
		return new Implementation<>(
			requireNonNull(authenticator),
			requireNonNull(authorizationManager)
		);
	}



	/**
	 * Default {@link SecurityManager} implementation that wraps delegate {@link Authenticator} and
	 * {@link AuthorizationManager} instances.
	 * <p>
	 * This implementation is immutable.
	 *
	 * @param <C> the type of the credentials instance to be authenticated.
	 * @param <R> the type of the result/response instance to be returned upon an authentication attempt.
	 *
	 * @author XDEV Software (TM)
	 */
	public final class Implementation<C, R> implements SecurityManager<C, R>
	{
		///////////////////////////////////////////////////////////////////////////
		// instance fields //
		////////////////////

		private final Authenticator<C, R>  authenticator       ;
		private final AuthorizationManager authorizationManager;



		///////////////////////////////////////////////////////////////////////////
		// constructors //
		/////////////////

		/**
		 * Implementation detail constructor that might change in the future.
		 */
		Implementation(
			final Authenticator<C, R>  authenticator       ,
			final AuthorizationManager authorizationManager
		)
		{
			super();
			this.authenticator        = authenticator       ;
			this.authorizationManager = authorizationManager;
		}



		///////////////////////////////////////////////////////////////////////////
		// override methods //
		/////////////////////

		/**
		 * {@inheritDoc}
		 */
		@Override
		public final R authenticate(final C credentials) throws AuthenticationFailedException
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
