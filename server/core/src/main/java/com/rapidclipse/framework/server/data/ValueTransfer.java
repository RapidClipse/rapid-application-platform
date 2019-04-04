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
