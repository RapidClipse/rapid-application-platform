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

package com.rapidclipse.framework.server.security.authorization;

import static java.util.Objects.requireNonNull;

import java.util.LinkedHashMap;
import java.util.Map;

import com.rapidclipse.framework.security.authorization.Permission;
import com.rapidclipse.framework.security.authorization.Resource;
import com.rapidclipse.framework.security.authorization.Subject;
import com.vaadin.flow.component.Component;


/**
 * A component extension which evaluates a passed subject and reacts
 * accordingly. Which of the subject's information is used and what exactly the
 * reaction will be is of course implementation- specific, however the basic
 * intention of this type is to evaluate the subject has sufficient permission
 * for {@link Resource}s associated with a component and react accordingly (e.g.
 * by making the component invisible).
 *
 * @author XDEV Software
 */
@FunctionalInterface
public interface SubjectEvaluatingComponentExtension
{
	public static interface Builder
	{
		public Builder add(final Resource resource, final SubjectEvaluationStrategy strategy);
		
		public SubjectEvaluatingComponentExtension build();
		
		public static Builder New()
		{
			return new Implementation();
		}
		
		public static class Implementation implements Builder
		{
			protected final Map<Resource, SubjectEvaluationStrategy> resourceStrategies;
			
			public Implementation()
			{
				super();
				
				this.resourceStrategies = new LinkedHashMap<>();
			}
			
			@Override
			public Builder add(final Resource resource, final SubjectEvaluationStrategy strategy)
			{
				requireNonNull(resource);
				requireNonNull(strategy);
				
				this.resourceStrategies.put(resource, strategy);
				
				return this;
			}
			
			@Override
			public SubjectEvaluatingComponentExtension build()
			{
				return SubjectEvaluatingComponentExtension.New(this.resourceStrategies);
			}
		}
	}
	
	/**
	 * Evaluates the passed {@link Subject} instance by checking if it has
	 * sufficient {@link Permission}s for the component's {@link Resource}s.
	 *
	 * @param component
	 * @param subject
	 *            the {@link Subject} instance to be evaluated.
	 */
	public void evaluateSubject(final Component component, final Subject subject);
	
	public static SubjectEvaluatingComponentExtension New(
		final Resource resource,
		final SubjectEvaluationStrategy strategy)
	{
		return Builder.New().add(resource, strategy).build();
	}
	
	public static SubjectEvaluatingComponentExtension New(
		final Map<Resource, SubjectEvaluationStrategy> resourceStrategies)
	{
		return new Implementation(resourceStrategies);
	}
	
	public static class Implementation implements SubjectEvaluatingComponentExtension
	{
		protected final Map<Resource, SubjectEvaluationStrategy> resourceStrategies;
		
		protected Implementation(final Map<Resource, SubjectEvaluationStrategy> resourceStrategies)
		{
			requireNonNull(resourceStrategies);
			
			this.resourceStrategies = resourceStrategies;
		}
		
		@Override
		public void evaluateSubject(final Component component, final Subject subject)
		{
			this.resourceStrategies.entrySet().forEach(entry -> {
				final Resource                  resource      = entry.getKey();
				final SubjectEvaluationStrategy strategy      = entry.getValue();
				final boolean                   hasPermission = subject.hasPermission(resource);
				strategy.subjectEvaluated(component, hasPermission);
			});
		}
	}
}
