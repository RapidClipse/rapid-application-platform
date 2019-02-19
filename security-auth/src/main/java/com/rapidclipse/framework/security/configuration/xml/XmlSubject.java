
package com.rapidclipse.framework.security.configuration.xml;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;


/**
 * JAXB mapping type.
 *
 * @author XDEV Software (TM)
 */
public final class XmlSubject
{
	///////////////////////////////////////////////////////////////////////////
	// instance fields //
	////////////////////
	
	@XmlAttribute
	String                      name;
	
	@XmlAttribute
	String                      password;
	
	@XmlElement(name = "role")
	ArrayList<XmlRoleReference> roles;
	
	///////////////////////////////////////////////////////////////////////////
	// constructors //
	/////////////////
	
	public XmlSubject(final String name, final String password, final ArrayList<XmlRole> roles)
	{
		super();
		this.name     = name;
		this.password = password;
		this.roles    = XmlRoleReference.box(roles);
	}
	
	public XmlSubject(final String name, final ArrayList<XmlRole> roles)
	{
		this(name, null, roles);
	}
	
	// JAXB dummy constructor
	XmlSubject()
	{
		this(null, null, null);
	}
	
	///////////////////////////////////////////////////////////////////////////
	// override methods //
	/////////////////////
	
	public final String password()
	{
		return this.password;
	}
	
	public final List<XmlRoleReference> roles()
	{
		return this.roles;
	}
	
	///////////////////////////////////////////////////////////////////////////
	// override methods //
	/////////////////////
	
	public final String name()
	{
		return this.name;
	}
	
}
