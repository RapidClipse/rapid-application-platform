
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
