/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
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
