/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
 *
 * This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * which accompanies this distribution, and is available at
 * https://www.eclipse.org/legal/epl-2.0/
 *
 * SPDX-License-Identifier: EPL-2.0
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 */
package com.rapidclipse.framework.server.security.authentication;

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
	private final static String AUTHENTICATION_RESULT = "AUTHENTICATION_RESULT";

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
				subject = new Subject.Implementation(credentials.username());
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

	/**
	 * Navigates to the application's redirect view.
	 * <p>
	 * Used after a successful login.
	 *
	 * @see RedirectView
	 * @see #login(Subject, Object)
	 */
	public static void navigateToRedirectView()
	{
		Navigation.navigateTo(RedirectView.class);
	}

	public static void rerouteToLoginView(final BeforeEvent event)
	{
		Navigation.rerouteTo(event, LoginView.class);
	}

	public static void rerouteToRedirectView(final BeforeEvent event)
	{
		Navigation.rerouteTo(event, RedirectView.class);
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
		Authentication.unauthenticatedNavigationRequestHandler = unauthenticatedNavigationRequestHandler;
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
