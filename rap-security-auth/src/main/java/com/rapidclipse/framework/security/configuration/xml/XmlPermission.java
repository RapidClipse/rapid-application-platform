/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.security.configuration.xml;

import jakarta.xml.bind.annotation.XmlAttribute;


/**
 * JAXB mapping type.
 *
 * @author XDEV Software (TM)
 */
public final class XmlPermission
{
	///////////////////////////////////////////////////////////////////////////
	// instance fields //
	////////////////////

	@XmlAttribute
	String  resource;

	@XmlAttribute
	Integer factor;

	///////////////////////////////////////////////////////////////////////////
	// constructors //
	/////////////////

	public XmlPermission(final XmlResource resource, final Integer factor)
	{
		super();
		this.resource = resource == null ? null : resource.name;
		this.factor   = factor;
	}

	// JAXB dummy constructor
	XmlPermission()
	{
		this(null, null);
	}

	///////////////////////////////////////////////////////////////////////////
	// declared methods //
	/////////////////////

	public final String resource()
	{
		return this.resource;
	}

	public final Integer factor()
	{
		return this.factor;
	}

	///////////////////////////////////////////////////////////////////////////
	// override methods //
	/////////////////////

	@Override
	public final String toString()
	{
		return this.resource + (this.factor == 0 ? "" : "(" + this.factor + ")");
	}

}
