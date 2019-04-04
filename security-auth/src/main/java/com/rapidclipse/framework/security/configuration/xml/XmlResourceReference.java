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

import java.util.ArrayList;

import javax.xml.bind.annotation.XmlAttribute;

import com.rapidclipse.framework.security.util.Named;


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
