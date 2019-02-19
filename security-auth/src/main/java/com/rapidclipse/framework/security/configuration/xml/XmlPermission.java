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

package com.rapidclipse.framework.security.configuration.xml;

import javax.xml.bind.annotation.XmlAttribute;


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
