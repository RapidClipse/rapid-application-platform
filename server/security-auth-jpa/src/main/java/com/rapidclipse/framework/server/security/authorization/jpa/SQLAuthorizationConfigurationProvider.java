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

package com.rapidclipse.framework.server.security.authorization.jpa;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import javax.persistence.EntityManager;
import javax.persistence.Query;

import com.rapidclipse.framework.security.authorization.AuthorizationConfiguration;
import com.rapidclipse.framework.security.authorization.AuthorizationConfigurationProvider;
import com.rapidclipse.framework.server.jpa.Jpa;


/**
 *
 * @author XDEV Software (CK)
 */
public class SQLAuthorizationConfigurationProvider implements AuthorizationConfigurationProvider
{
	private final String usersAndGroupsSelect;
	private final String rolesAndPermissionsSelect;
	private final String persistenceUnit;
	
	public SQLAuthorizationConfigurationProvider(
		final String usersAndGroupsSelect,
		final String rolesAndPermissionsSelect)
	{
		this(usersAndGroupsSelect, rolesAndPermissionsSelect,
			Jpa.getPersistenceManager().getDefaultPersistenceUnit());
	}
	
	public SQLAuthorizationConfigurationProvider(
		final String usersAndGroupsSelect,
		final String rolesAndPermissionsSelect,
		final String persistenceUnit)
	{
		this.usersAndGroupsSelect      = usersAndGroupsSelect;
		this.rolesAndPermissionsSelect = rolesAndPermissionsSelect;
		this.persistenceUnit           = persistenceUnit;
	}
	
	/**
	 * @return the usersAndGroupsSelect
	 */
	public String getUsersAndGroupsSelect()
	{
		return this.usersAndGroupsSelect;
	}
	
	/**
	 * @return the rolesAndPermissionsSelect
	 */
	public String getRolesAndPermissionsSelect()
	{
		return this.rolesAndPermissionsSelect;
	}
	
	/**
	 * @return the persistenceUnit
	 */
	public String getPersistenceUnit()
	{
		return this.persistenceUnit;
	}
	
	/**
	 * {@inheritDoc}
	 */
	@SuppressWarnings("unchecked")
	@Override
	public AuthorizationConfiguration provideConfiguration()
	{
		final Map<String, Set<String>>          resourceResources   = new HashMap<>();
		final Map<String, Set<String>>          roleRoles           = new HashMap<>();
		final Map<String, Map<String, Integer>> rolePermissions     = new HashMap<>();
		final Map<String, Set<String>>          subjectRoles        = new HashMap<>();
		
		final EntityManager                     entityManager       = Jpa.getEntityManager(getPersistenceUnit());
		final Query                             usersAndGroupsQuery = entityManager
			.createNativeQuery(getUsersAndGroupsSelect());
		final List<Object[]>                    usersAndGroupsList  = usersAndGroupsQuery.getResultList();
		
		usersAndGroupsList.stream().map(row -> String.valueOf(row[0]))
			.filter(user -> !subjectRoles.containsKey(user))
			.forEach(user -> subjectRoles.put(user, unboxGroups(user, usersAndGroupsList)));
		
		final Query          rolesAndPermissionsQuery = entityManager
			.createNativeQuery(getRolesAndPermissionsSelect());
		final List<Object[]> rolesAndPermissionsList  = rolesAndPermissionsQuery.getResultList();
		
		rolesAndPermissionsList.stream().map(row -> String.valueOf(row[0]))
			.filter(role -> !rolePermissions.containsKey(role)).forEach(role -> rolePermissions
				.put(role, unboxPermissions(role, rolesAndPermissionsList)));
		
		rolePermissions.forEach((role, permissions) -> {
			roleRoles.put(role, Collections.EMPTY_SET);
			permissions.keySet()
				.forEach(permission -> resourceResources.put(permission, Collections.EMPTY_SET));
		});
		
		subjectRoles.values()
			.forEach(roles -> roles.forEach(role -> roleRoles.put(role, Collections.EMPTY_SET)));
		
		return AuthorizationConfiguration.New(resourceResources, roleRoles, rolePermissions,
			subjectRoles);
	}
	
	protected Set<String> unboxGroups(final String username, final List<Object[]> rows)
	{
		return rows.stream().filter(row -> username.equals(row[0])).map(row -> (String)row[1])
			.collect(Collectors.toSet());
	}
	
	protected Map<String, Integer> unboxPermissions(
		final String groupName,
		final List<Object[]> rows)
	{
		return rows.stream().filter(row -> groupName.equals(row[0])).map(row -> (String)row[1])
			.collect(Collectors.toMap(permission -> permission, permission -> 1));
	}
}
