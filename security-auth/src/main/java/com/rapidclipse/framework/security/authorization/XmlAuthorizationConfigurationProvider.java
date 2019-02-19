/*-
 * ---
 * Rapid Application Platform / Security / Authentication and Authorization
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

package com.rapidclipse.framework.security.authorization;

import static java.util.Objects.requireNonNull;

import java.io.File;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;

import com.rapidclipse.framework.security.configuration.xml.XmlConfiguration;
import com.rapidclipse.framework.security.configuration.xml.XmlPermission;
import com.rapidclipse.framework.security.configuration.xml.XmlResource;
import com.rapidclipse.framework.security.configuration.xml.XmlRole;
import com.rapidclipse.framework.security.configuration.xml.XmlSubject;
import com.rapidclipse.framework.security.util.Named;


public class XmlAuthorizationConfigurationProvider implements AuthorizationConfigurationProvider
{
	///////////////////////////////////////////////////////////////////////////
	// static methods //
	///////////////////
	
	public static final AuthorizationConfiguration readConfiguration(final File xmlFile) throws AuthorizationException
	{
		final XmlConfiguration xmlConfig = XmlConfiguration.readFromFile(xmlFile);
		return build(xmlConfig);
	}
	
	public static final AuthorizationConfiguration build(final XmlConfiguration xmlConfig)
	{
		final HashMap<String, HashSet<String>>          resourceResources = new HashMap<>();
		final HashMap<String, HashSet<String>>          roleRoles         = new HashMap<>();
		final HashMap<String, HashMap<String, Integer>> rolePermissions   = new HashMap<>();
		final HashMap<String, HashSet<String>>          subjectRoles      = new HashMap<>();
		
		for(final XmlResource resource : xmlConfig.resources())
		{
			resourceResources.put(resource.name(), unboxNames(resource.children()));
		}
		
		for(final XmlRole role : xmlConfig.roles())
		{
			// must put role name even if associated collections are null to register the role itself
			roleRoles.put(role.name(), unboxNames(role.roles()));
			rolePermissions.put(role.name(), unboxPermissions(role.permissions()));
		}
		
		for(final XmlSubject subject : xmlConfig.subjects())
		{
			subjectRoles.put(subject.name(), unboxNames(subject.roles()));
		}
		
		// (25.06.2014 TM)TODO: validate referential integrity at string level here?
		
		return AuthorizationConfiguration.New(resourceResources, roleRoles, rolePermissions, subjectRoles);
	}
	
	private static HashSet<String> unboxNames(final List<? extends Named> nameds)
	{
		if(nameds == null)
		{
			return null;
		}
		
		final HashSet<String> names = new HashSet<>(nameds.size());
		
		for(final Named named : nameds)
		{
			names.add(named.name());
		}
		
		return names;
	}
	
	private static HashMap<String, Integer> unboxPermissions(final List<XmlPermission> permissions)
	{
		if(permissions == null)
		{
			return null;
		}
		
		final HashMap<String, Integer> unboxed = new HashMap<>();
		
		for(final XmlPermission permission : permissions)
		{
			unboxed.put(permission.resource(), permission.factor() == null ? 0 : permission.factor());
		}
		
		return unboxed;
	}
	
	public static final XmlAuthorizationConfigurationProvider New(final File xmlFile)
	{
		return new XmlAuthorizationConfigurationProvider(
			requireNonNull(xmlFile));
	}
	
	///////////////////////////////////////////////////////////////////////////
	// instance fields //
	////////////////////
	
	private final File xmlFile;
	
	///////////////////////////////////////////////////////////////////////////
	// constructors //
	/////////////////
	
	/**
	 * Implementation detail constructor that might change in the future.
	 */
	XmlAuthorizationConfigurationProvider(final File xmlFile)
	{
		super();
		this.xmlFile = xmlFile;
	}
	
	///////////////////////////////////////////////////////////////////////////
	// override methods //
	/////////////////////
	
	/**
	 * {@inheritDoc}
	 */
	@Override
	public AuthorizationConfiguration provideConfiguration()
	{
		return readConfiguration(this.xmlFile);
	}
	
}
