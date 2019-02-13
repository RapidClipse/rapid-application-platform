/*
 * Copyright (C) 2013-2018 by XDEV Software, All Rights Reserved.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 *
 * For further information see
 * <http://www.rapidclipse.com/en/legal/license/license.html>.
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
			return new String(value,StandardCharsets.UTF_8);
		}
		
		return null;
	}
}
