/*
 * Copyright (C) 2013-2022 by XDEV Software, All Rights Reserved.
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
package com.rapidclipse.framework.server.security.authorization;

import static java.util.Objects.requireNonNull;

import java.util.Collection;

import com.rapidclipse.framework.security.authorization.AuthorizationManager;
import com.rapidclipse.framework.security.authorization.Resource;
import com.rapidclipse.framework.security.authorization.Subject;
import com.rapidclipse.framework.server.navigation.Navigation;
import com.rapidclipse.framework.server.security.authentication.Authentication;
import com.rapidclipse.framework.server.ui.UIUtils;
import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.ComponentUtil;
import com.vaadin.flow.component.UI;
import com.vaadin.flow.router.BeforeEvent;


/**
 * Utility class for authorization purposes.
 *
 * @author XDEV Software
 *
 */
public final class Authorization
{
	/**
	 * Registers an {@link AuthorizationManager} in the current user session.
	 *
	 * @param authorizationManager
	 *            the {@link AuthorizationManager} to register
	 * @see #getAuthorizationManager()
	 */
	public static void setAuthorizationManager(final AuthorizationManager authorizationManager)
	{
		UI.getCurrent().getSession().setAttribute(AuthorizationManager.class, authorizationManager);
	}

	/**
	 * Returns the {@link AuthorizationManager} of the current user session.
	 *
	 * @return the {@link AuthorizationManager}
	 * @see #setAuthorizationManager(AuthorizationManager)
	 */
	public static AuthorizationManager getAuthorizationManager()
	{
		return UI.getCurrent().getSession().getAttribute(AuthorizationManager.class);
	}

	/**
	 * Searches for a specific resource in the current
	 * {@link AuthorizationManager}. If no resource is found an
	 * {@link IllegalArgumentException} is thrown.
	 *
	 * @param name
	 *            the resource's name to search for
	 * @return the resource
	 * @throws IllegalStateException
	 *             if no authorization manager has been initialized
	 * @throws IllegalArgumentException
	 *             if the resources couldn't be found
	 * @see #resource(String)
	 */
	public static Resource getResource(final String name)
		throws IllegalStateException, IllegalArgumentException
	{
		final AuthorizationManager authorizationManager = getAuthorizationManager();
		if(authorizationManager == null)
		{
			throw new IllegalStateException("No authorization manager has been initialized");
		}
		final Resource resource = authorizationManager.resource(name);
		if(resource == null)
		{
			throw new IllegalArgumentException("Resource not found: " + name);
		}
		return resource;
	}

	/**
	 * Searches for a specific resource with {@link #getResource(String)}. If no
	 * resource with the specific name is present a new one will be created and
	 * returned.
	 *
	 * @param name
	 *            the resource's name
	 * @return the found or created resource
	 * @see #getResource(String)
	 */
	public static Resource resource(final String name)
	{
		try
		{
			return getResource(name);
		}
		catch(IllegalStateException | IllegalArgumentException e)
		{
			return Resource.New(name);
		}
	}

	/**
	 * Registers a {@link SubjectEvaluatingComponentExtension} with the
	 * <code>component</code>.
	 *
	 * @param component
	 *            the component to register the extension with
	 * @param extension
	 *            the extension to register
	 * @see #evaluateComponents(Component)
	 */
	public static void setSubjectEvaluatingComponentExtension(
		final Component component,
		final SubjectEvaluatingComponentExtension extension)
	{
		ComponentUtil.setData(component, SubjectEvaluatingComponentExtension.class, extension);
	}

	/**
	 * Returns the registered {@link SubjectEvaluatingComponentExtension} or <code>null</code>.
	 *
	 * @param component
	 * @return
	 * @see #setSubjectEvaluatingComponentExtension(Component, SubjectEvaluatingComponentExtension)
	 */
	public static SubjectEvaluatingComponentExtension getSubjectEvaluatingComponentExtension(
		final Component component)
	{
		return ComponentUtil.getData(component, SubjectEvaluatingComponentExtension.class);
	}

	/**
	 * Copies the registered resources from the <code>routedType</code> as a {@link SubjectEvaluatingComponentExtension}
	 * with the given <code>strategy</code> into the <code>component</code>.
	 * If the routed type has no resources this is a no-op.
	 *
	 * @param routedType
	 * @param component
	 * @param strategy
	 * @see RouteResourcesProvider
	 * @see #setSubjectEvaluatingComponentExtension(Component, SubjectEvaluatingComponentExtension)
	 */
	public static void
		copyResources(final Class<?> routedType, final Component component, final SubjectEvaluationStrategy strategy)
	{
		final Collection<Resource> resources = getRouteResourcesProvider().getResourcesFor(routedType);
		if(!resources.isEmpty())
		{
			final SubjectEvaluatingComponentExtension.Builder builder = SubjectEvaluatingComponentExtension.Builder();
			resources.forEach(r -> builder.add(r, strategy));
			setSubjectEvaluatingComponentExtension(component, builder.build());
		}
	}

	/**
	 * Evaluates all {@link SubjectEvaluatingComponentExtension}s in the
	 * component hierarchy of <code>root</code> against the current user.
	 *
	 * @param root
	 *            the root component
	 * @see Authentication#isUserLoggedIn()
	 * @see Authentication#login(Subject, Object)
	 * @see Authentication#setUser(Subject, Object)
	 * @see Authentication#getUser()
	 * @see #setSubjectEvaluatingComponentExtension(Component,
	 *      SubjectEvaluatingComponentExtension)
	 */
	public static void evaluateComponents(final Component root)
	{
		if(Authentication.isUserLoggedIn())
		{
			evaluateComponents(root, Authentication.getUser());
		}
	}

	/**
	 * Evaluates all {@link SubjectEvaluatingComponentExtension}s in the
	 * component hierarchy of <code>root</code> against the given
	 * <code>subject</code>.
	 *
	 * @param root
	 *            the root component
	 * @param subject
	 *            the subject to evaluate against
	 * @see #evaluateComponents(Component)
	 * @see #setSubjectEvaluatingComponentExtension(Component,
	 *      SubjectEvaluatingComponentExtension)
	 */
	public static void evaluateComponents(final Component root, final Subject subject)
	{
		UIUtils.lookupComponentTree(root, component -> {

			evaluateComponent(component, subject);

			return null;
		});
	}

	/**
	 * Evaluates the {@link SubjectEvaluatingComponentExtension} of the
	 * <code>component</code> against the given <code>subject</code>.
	 * <p>
	 * If no {@link SubjectEvaluatingComponentExtension} has been registered
	 * with the <code>component</code> this method has no effect.
	 *
	 * @param component
	 *            the component to check
	 * @param subject
	 *            the subject to evaluate against
	 * @see #setSubjectEvaluatingComponentExtension(Component,
	 *      SubjectEvaluatingComponentExtension)
	 */
	public static void evaluateComponent(final Component component, final Subject subject)
	{
		final SubjectEvaluatingComponentExtension extension;
		if((extension = getSubjectEvaluatingComponentExtension(component)) != null)
		{
			extension.evaluateSubject(component, subject);
		}
	}

	/**
	 * Navigates to the application's permission denied view.
	 *
	 * @see PermissionDeniedView
	 */
	public static void navigateToPermissionDeniedView()
	{
		Navigation.navigateTo(PermissionDeniedView.class);
	}

	public static void rerouteToPermissionDeniedView(final BeforeEvent event)
	{
		Navigation.rerouteTo(event, PermissionDeniedView.class);
	}

	private static UnauthorizedNavigationRequestHandler unauthorizedNavigationRequestHandler =
		UnauthorizedNavigationRequestHandler
			.Default();

	/**
	 * @param unauthorizedNavigationRequestHandler
	 *            the unauthorizedNavigationRequestHandler to set
	 */
	public static void setUnauthorizedNavigationRequestHandler(
		final UnauthorizedNavigationRequestHandler unauthorizedNavigationRequestHandler)
	{
		Authorization.unauthorizedNavigationRequestHandler = requireNonNull(unauthorizedNavigationRequestHandler);
	}

	/**
	 * @return the unauthorizedNavigationRequestHandler
	 */
	public static UnauthorizedNavigationRequestHandler getUnauthorizedNavigationRequestHandler()
	{
		return unauthorizedNavigationRequestHandler;
	}

	private static RouteResourcesProvider routeResourcesProvider = RouteResourcesProvider.Default();

	/**
	 * @param routeResourcesProvider
	 *            the routeResourcesProvider to set
	 */
	public static void setRouteResourcesProvider(final RouteResourcesProvider routeResourcesProvider)
	{
		Authorization.routeResourcesProvider = requireNonNull(routeResourcesProvider);
	}

	/**
	 * @return the routeResourcesProvider
	 */
	public static RouteResourcesProvider getRouteResourcesProvider()
	{
		return routeResourcesProvider;
	}

	private Authorization()
	{
		throw new Error();
	}
}
