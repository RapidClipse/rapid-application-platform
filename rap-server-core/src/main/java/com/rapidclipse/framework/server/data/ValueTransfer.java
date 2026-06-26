/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.data;

import com.rapidclipse.framework.server.util.ServiceLoader;


/**
 * @author XDEV Software
 *
 */
public final class ValueTransfer
{
	public static Object put(final Object value)
	{
		return ServiceLoader.forType(ValueTransferHandler.class).servicesStream()
			.filter(handler -> handler.handlesPut(value)).map(handler -> handler.put(value))
			.findFirst().orElse(value);
	}
	
	public static Object get(final Object value)
	{
		return ServiceLoader.forType(ValueTransferHandler.class).servicesStream()
			.filter(handler -> handler.handlesGet(value)).map(handler -> handler.get(value))
			.findFirst().orElse(value);
	}
	
	private ValueTransfer()
	{
		throw new Error();
	}
}
