/*
 * Copyright (C) 2013-2023 by XDEV Software, All Rights Reserved.
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
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software - initial API and implementation
 */
package com.rapidclipse.framework.server.security.authentication;

import static java.util.Objects.requireNonNull;

import com.rapidclipse.framework.security.authentication.AuthenticationFailedException;
import com.rapidclipse.framework.security.authentication.AuthenticatorProvider;
import com.rapidclipse.framework.security.authentication.CredentialsUsernamePassword;
import com.rapidclipse.framework.security.authorization.AuthorizationConfigurationProvider;
import com.rapidclipse.framework.security.authorization.AuthorizationManager;
import com.rapidclipse.framework.security.authorization.Subject;
import com.rapidclipse.framework.server.navigation.Navigation;
import com.rapidclipse.framework.server.security.authorization.Authorization;
import com.vaadin.flow.router.BeforeEvent;
import com.vaadin.flow.server.VaadinSession;


/**
 * Utility class for authentication purposes.
 *
 * @author XDEV Software
 *
 */
public final class Authentication
{
	private final static String AUTHENTICATION_RESULT = Authentication.class.getName() + ".AUTHENTICATION_RESULT";
	private final static String REDIRECT_VIEW         = Authentication.class.getName() + ".REDIRECT_VIEW";
	
	/**
	 * A login with the given credentials is attempted. If successful the user
	 * is registered in the current session and then the redirect view will be
	 * shown.
	 *
	 * @return <code>true</code> if the login was successful
	 *
	 * @see #login(Subject, Object)
	 */
	public static <C extends CredentialsUsernamePassword> boolean tryLogin(
		final C credentials,
		final AuthenticatorProvider<C, ?> authenticatorProvider)
	{
		return tryLogin(credentials, authenticatorProvider, null);
	}
	
	/**
	 * A login with the given credentials is attempted. If successful the user
	 * is registered in the current session and then the redirect view will be
	 * shown.
	 *
	 * @return <code>true</code> if the login was successful
	 *
	 * @see #login(Subject, Object)
	 */
	public static <C extends CredentialsUsernamePassword> boolean tryLogin(
		final C credentials,
		final AuthenticatorProvider<C, ?> authenticatorProvider,
		final AuthorizationConfigurationProvider authorizationConfigurationProvider)
	{
		try
		{
			final Object authenticationResult = authenticatorProvider.provideAuthenticator()
				.authenticate(credentials);
			Subject      subject;
			if(authorizationConfigurationProvider != null)
			{
				final AuthorizationManager authorizationManager = AuthorizationManager
					.New(authorizationConfigurationProvider);
				Authorization.setAuthorizationManager(authorizationManager);
				subject = authorizationManager.subject(credentials.username());
			}
			else
			{
				subject = Subject.New(credentials.username());
			}
			login(subject, authenticationResult);
			return true;
		}
		catch(final AuthenticationFailedException e)
		{
			return false;
		}
	}
	
	/**
	 * Registers the <code>user</code> with the
	 * <code>authenticationResult</code> in the current session and navigates to
	 * the redirect view.
	 *
	 * @param user
	 *            the current user
	 * @param authenticationResult
	 *            the authentication's result
	 *
	 * @see #setUser(Subject, Object)
	 * @see #navigateToRedirectView()
	 */
	public static void login(final Subject user, final Object authenticationResult)
	{
		setUser(user, authenticationResult);
		navigateToRedirectView();
	}
	
	/**
	 * Removes the current user from the current session and redirects to the
	 * login view.
	 *
	 * @see #setUser(Subject, Object)
	 * @see #navigateToLoginView()
	 */
	public static void logout()
	{
		setUser(null, null);
		navigateToLoginView();
	}
	
	/**
	 * Registers the <code>user</code> with the
	 * <code>authenticationResult</code> in the current session.
	 *
	 * @param user
	 *            the current user
	 * @param authenticationResult
	 *            the authentication's result
	 *
	 * @see #getUser()
	 */
	public static void setUser(final Subject user, final Object authenticationResult)
	{
		final VaadinSession session = VaadinSession.getCurrent();
		session.setAttribute(Subject.class, user);
		session.setAttribute(AUTHENTICATION_RESULT, authenticationResult);
	}
	
	/**
	 * Returns the current user, which was registered with
	 * {@link #setUser(Subject, Object)}.
	 *
	 * @return the current user or <code>null</code> if no user is currently
	 *         registered.
	 */
	public static Subject getUser()
	{
		return VaadinSession.getCurrent().getAttribute(Subject.class);
	}
	
	/**
	 * Returns the result of the last authentification of
	 * {@link #setUser(Subject, Object)}.
	 *
	 * @return the current authentication result or <code>null</code> if no user
	 *         is currently registered.
	 */
	public static Object getAuthenticationResult()
	{
		return VaadinSession.getCurrent().getAttribute(AUTHENTICATION_RESULT);
	}
	
	/**
	 * Returns <code>true</code> if a user is registered in the current session,
	 * <code>false</code> otherwise.
	 *
	 * @return the current user
	 *
	 * @see #setUser(Subject, Object)
	 * @see #login(Subject, Object)
	 * @see #logout()
	 */
	public static boolean isUserLoggedIn()
	{
		return getUser() != null;
	}
	
	/**
	 * Navigates to the application's {@link LoginView}.
	 *
	 * @see LoginView
	 * @see #logout()
	 */
	public static void navigateToLoginView()
	{
		Navigation.navigateTo(LoginView.class);
	}
	
	public static void rerouteToLoginView(final BeforeEvent event)
	{
		setRedirectView(event.getNavigationTarget());

		Navigation.rerouteTo(event, LoginView.class);
	}
	
	/**
	 * Navigates to the application's redirect view.
	 * <p>
	 * Used after a successful login.
	 *
	 * @see RedirectView
	 * @see #setRedirectView(Class)
	 * @see #login(Subject, Object)
	 */
	public static void navigateToRedirectView()
	{
		Navigation.navigateTo(getAndClearRedirectView());
	}
	
	public static void rerouteToRedirectView(final BeforeEvent event)
	{
		Navigation.rerouteTo(event, getAndClearRedirectView());
	}
	
	public static void setRedirectView(final Class<?> redirectView)
	{
		VaadinSession.getCurrent().setAttribute(REDIRECT_VIEW, redirectView);
	}

	private static Class<?> getAndClearRedirectView()
	{
		final VaadinSession session   = VaadinSession.getCurrent();
		final Object        attribute = session.getAttribute(REDIRECT_VIEW);
		session.setAttribute(REDIRECT_VIEW, null);
		return attribute instanceof Class<?>
			? (Class<?>)attribute
			: RedirectView.class;
	}
	
	private static UnauthenticatedNavigationRequestHandler unauthenticatedNavigationRequestHandler =
		UnauthenticatedNavigationRequestHandler
			.Default();
	
	/**
	 * @param unauthenticatedNavigationRequestHandler
	 *            the unauthenticatedNavigationRequestHandler to set
	 */
	public static void setUnauthenticatedNavigationRequestHandler(
		final UnauthenticatedNavigationRequestHandler unauthenticatedNavigationRequestHandler)
	{
		Authentication.unauthenticatedNavigationRequestHandler =
			requireNonNull(unauthenticatedNavigationRequestHandler);
	}
	
	/**
	 * @return the unauthenticatedNavigationRequestHandler
	 */
	public static UnauthenticatedNavigationRequestHandler getUnauthenticatedNavigationRequestHandler()
	{
		return unauthenticatedNavigationRequestHandler;
	}
	
	private Authentication()
	{
		throw new Error();
	}
}
