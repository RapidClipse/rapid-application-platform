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

package com.rapidclipse.framework.security.configuration.xml;

import java.util.ArrayList;

import com.rapidclipse.framework.security.util.Named;

import jakarta.xml.bind.annotation.XmlAttribute;


// pragmatic workaround class for insufficient JAXB concept
/**
 * JAXB mapping type.
 *
 * @author XDEV Software (TM)
 */
public final class XmlRoleReference implements Named
{
	///////////////////////////////////////////////////////////////////////////
	// static methods //
	///////////////////

	public static final ArrayList<XmlRoleReference> box(final ArrayList<XmlRole> roles)
	{
		if(roles == null)
		{
			return null;
		}

		final ArrayList<XmlRoleReference> names = new ArrayList<>(roles.size());

		for(final XmlRole role : roles)
		{
			names.add(new XmlRoleReference(role.name));
		}

		return names;
	}

	///////////////////////////////////////////////////////////////////////////
	// instance fields //
	////////////////////

	@XmlAttribute
	String name;

	///////////////////////////////////////////////////////////////////////////
	// constructors //
	/////////////////

	public XmlRoleReference(final String name)
	{
		super();
		this.name = name;
	}

	// JAXB dummy constructor
	XmlRoleReference()
	{
		this(null);
	}

	///////////////////////////////////////////////////////////////////////////
	// override methods //
	/////////////////////

	@Override
	public final String name()
	{
		return this.name;
	}

}
