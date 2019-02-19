/*-
 * ---
 * Rapid Application Platform / Server / Core
 * --
 * Copyright (C) 2013 - 2019 XDEV Software Corp.
 * --
 * This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * which accompanies this distribution, and is available at
 * https://www.eclipse.org/legal/epl-2.0/
 * 
 * SPDX-License-Identifier: EPL-2.0
 * 
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 * ---
 */

package com.rapidclipse.framework.server.navigation;

import static java.util.Collections.unmodifiableMap;
import static java.util.Objects.requireNonNull;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;


/**
 * @author XDEV Software
 *
 */
public interface NavigationParametersMetadata
{
	public Iterable<String> names();
	
	public NavigationParameterMetadata get(final String name);
	
	public List<String> mandatoryParameters();
	
	public static NavigationParametersMetadata New(final Class<?> targetType)
	{
		return New(NavigationUtils.getMetadata(targetType));
	}
	
	public static NavigationParametersMetadata New(
		final Map<String, NavigationParameterMetadata> parameters)
	{
		return new Implementation(parameters);
	}
	
	public static class Implementation implements NavigationParametersMetadata
	{
		private final Map<String, NavigationParameterMetadata> parameters;
		
		public Implementation(final Map<String, NavigationParameterMetadata> parameters)
		{
			super();
			this.parameters = unmodifiableMap(requireNonNull(parameters));
		}
		
		@Override
		public Iterable<String> names()
		{
			return this.parameters.keySet();
		}
		
		@Override
		public NavigationParameterMetadata get(final String name)
		{
			return this.parameters.get(name);
		}
		
		@Override
		public List<String> mandatoryParameters()
		{
			return this.parameters.entrySet().stream().filter(e -> !e.getValue().optional())
				.map(e -> e.getKey()).collect(Collectors.toList());
		}
	}
}
