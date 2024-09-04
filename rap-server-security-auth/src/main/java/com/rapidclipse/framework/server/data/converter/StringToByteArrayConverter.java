/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.data.converter;

import java.nio.charset.StandardCharsets;

import com.vaadin.flow.data.binder.Result;
import com.vaadin.flow.data.binder.ValueContext;
import com.vaadin.flow.data.converter.Converter;


/**
 * @author XDEV Software
 */
public class StringToByteArrayConverter implements Converter<String, byte[]>
{
	@Override
	public Result<byte[]> convertToModel(final String value, final ValueContext context)
	{
		if(value != null)
		{
			return Result.ok(value.getBytes(StandardCharsets.UTF_8));
		}
		
		return Result.ok(null);
	}
	
	@Override
	public String convertToPresentation(final byte[] value, final ValueContext context)
	{
		if(value != null)
		{
			return new String(value, StandardCharsets.UTF_8);
		}
		
		return null;
	}
}
