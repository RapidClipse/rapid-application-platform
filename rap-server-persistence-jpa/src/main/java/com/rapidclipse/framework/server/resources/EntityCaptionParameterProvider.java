/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.resources;

import com.rapidclipse.framework.server.jpa.Jpa;


/**
 * @author XDEV Software
 *
 */
public class EntityCaptionParameterProvider implements CaptionParameterProvider
{
	public EntityCaptionParameterProvider()
	{
		super();
	}
	
	@Override
	public String getParameterValue(final Object element, final String parameterName)
	{
		final Object value = Jpa.resolveValue(element, parameterName);
		return String.valueOf(value);
	}
}
