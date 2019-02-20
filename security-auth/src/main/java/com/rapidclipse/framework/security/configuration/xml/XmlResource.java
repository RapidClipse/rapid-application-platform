
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
