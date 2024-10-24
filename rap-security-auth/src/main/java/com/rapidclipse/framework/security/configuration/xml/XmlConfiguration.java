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

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import com.rapidclipse.framework.security.authorization.AuthorizationException;

import jakarta.xml.bind.JAXBContext;
import jakarta.xml.bind.Unmarshaller;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;


/**
 * JAXB mapping type.
 *
 * @author XDEV Software (TM)
 */
@XmlRootElement(name = "security")
public final class XmlConfiguration
{
	///////////////////////////////////////////////////////////////////////////
	// static methods //
	///////////////////

	public static final XmlConfiguration readFromFile(final File xmlFile) throws AuthorizationException
	{
		try
		{
			final JAXBContext      jaxbContext      = JAXBContext.newInstance(XmlConfiguration.class);
			final Unmarshaller     jaxbUnmarshaller = jaxbContext.createUnmarshaller();
			final XmlConfiguration xmlConfig        = (XmlConfiguration)jaxbUnmarshaller.unmarshal(xmlFile);
			return xmlConfig;
		}
		catch(final Exception e)
		{
			// nothing to do here, just wrap in context-specific exception type.
			throw new AuthorizationException(e);
		}
	}

	///////////////////////////////////////////////////////////////////////////
	// instance fields //
	////////////////////

	@XmlElement(name = "resource")
	ArrayList<XmlResource> resources;

	@XmlElement(name = "role")
	ArrayList<XmlRole>     roles;

	@XmlElement(name = "subject")
	ArrayList<XmlSubject>  subjects;

	///////////////////////////////////////////////////////////////////////////
	// constructors //
	/////////////////

	public XmlConfiguration(
		final ArrayList<XmlResource> resources,
		final ArrayList<XmlRole> roles,
		final ArrayList<XmlSubject> subjects)
	{
		super();
		this.resources = resources;
		this.roles     = roles;
		this.subjects  = subjects;
	}

	// JAXB dummy constructor
	XmlConfiguration()
	{
		this(null, null, null);
	}

	///////////////////////////////////////////////////////////////////////////
	// declared methods //
	/////////////////////

	public final List<XmlResource> resources()
	{
		return this.resources;
	}

	public final List<XmlRole> roles()
	{
		return this.roles;
	}

	public final List<XmlSubject> subjects()
	{
		return this.subjects;
	}

	///////////////////////////////////////////////////////////////////////////
	// override methods //
	/////////////////////

	@Override
	public final String toString()
	{
		return this.getClass().getSimpleName() + " ("
			+ this.resources.size() + " resources, "
			+ this.roles.size() + " roles, "
			+ this.subjects.size() + " subjects)";
	}
}
