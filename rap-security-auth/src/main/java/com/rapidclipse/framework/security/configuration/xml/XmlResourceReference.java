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

import com.rapidclipse.framework.security.util.Named;

import jakarta.xml.bind.annotation.XmlAttribute;


// pragmatic workaround class for insufficient JAXB concept
/**
 * JAXB mapping type.
 *
 * @author XDEV Software (TM)
 */
public final class XmlResourceReference implements Named
{
	///////////////////////////////////////////////////////////////////////////
	// static methods //
	///////////////////

	public static final ArrayList<XmlResourceReference> box(final ArrayList<XmlResource> roles)
	{
		if(roles == null)
		{
			return null;
		}

		final ArrayList<XmlResourceReference> names = new ArrayList<>(roles.size());

		for(final XmlResource role : roles)
		{
			names.add(new XmlResourceReference(role.name));
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

	public XmlResourceReference(final String name)
	{
		super();
		this.name = name;
	}

	// JAXB dummy constructor
	XmlResourceReference()
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
