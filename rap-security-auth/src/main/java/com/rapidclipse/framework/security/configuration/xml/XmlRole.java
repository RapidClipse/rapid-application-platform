/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.security.configuration.xml;

import java.util.ArrayList;
import java.util.List;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;


/**
 * JAXB mapping type.
 *
 * @author XDEV Software (TM)
 */
public final class XmlRole
{
	///////////////////////////////////////////////////////////////////////////
	// instance fields //
	////////////////////

	@XmlAttribute
	String                      name;

	@XmlElement(name = "role")
	ArrayList<XmlRoleReference> roles;

	@XmlElement(name = "permission")
	ArrayList<XmlPermission>    permissions;

	///////////////////////////////////////////////////////////////////////////
	// constructors //
	/////////////////

	public XmlRole(final String name)
	{
		this(name, null, null);
	}

	public XmlRole(final String name, final ArrayList<XmlPermission> permissions)
	{
		this(name, null, permissions);
	}

	public XmlRole(final String name, final ArrayList<XmlRole> roles, final ArrayList<XmlPermission> permissions)
	{
		super();
		this.name        = name;
		this.roles       = XmlRoleReference.box(roles);
		this.permissions = permissions;
	}

	// JAXB dummy constructor
	XmlRole()
	{
		this(null, null, null);
	}

	///////////////////////////////////////////////////////////////////////////
	// declared methods //
	/////////////////////

	public final List<XmlRoleReference> roles()
	{
		return this.roles;
	}

	public final List<XmlPermission> permissions()
	{
		return this.permissions;
	}

	///////////////////////////////////////////////////////////////////////////
	// override methods //
	/////////////////////

	public final String name()
	{
		return this.name;
	}

}
