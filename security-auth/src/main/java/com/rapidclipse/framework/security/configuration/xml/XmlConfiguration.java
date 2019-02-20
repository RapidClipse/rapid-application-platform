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

package com.rapidclipse.framework.security.configuration.xml;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.Unmarshaller;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.rapidclipse.framework.security.authorization.AuthorizationException;


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
