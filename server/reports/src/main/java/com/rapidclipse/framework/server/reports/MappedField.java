
package com.rapidclipse.framework.server.reports;

import net.sf.jasperreports.engine.JRField;


/**
 * @author XDEV Software
 *
 */
public class MappedField extends DelegatingField
{
	private final String name;
	
	public MappedField(final JRField delegate, final String name)
	{
		super(delegate);
		
		this.name = name;
	}
	
	@Override
	public String getName()
	{
		return this.name;
	}
}
