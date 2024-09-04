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
public final class XmlResource
{
	///////////////////////////////////////////////////////////////////////////
	// instance fields //
	////////////////////

	@XmlAttribute
	String                          name;

	@XmlElement(name = "child")
	ArrayList<XmlResourceReference> children;

	///////////////////////////////////////////////////////////////////////////
	// constructors //
	/////////////////

	public XmlResource(final String name)
	{
		this(name, null);
	}

	public XmlResource(final String name, final ArrayList<XmlResource> children)
	{
		super();
		this.name     = name;
		this.children = XmlResourceReference.box(children);
	}

	// JAXB dummy constructor
	XmlResource()
	{
		this(null, null);
	}

	///////////////////////////////////////////////////////////////////////////
	// declared methods //
	/////////////////////

	public final List<XmlResourceReference> children()
	{
		return this.children;
	}

	///////////////////////////////////////////////////////////////////////////
	// override methods //
	/////////////////////

	public final String name()
	{
		return this.name;
	}

}
