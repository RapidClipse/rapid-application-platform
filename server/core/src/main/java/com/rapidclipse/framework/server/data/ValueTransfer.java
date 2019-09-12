/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
 *
 * This file is part of the RapidClipse Application Platform (RAP).
 *
 * RAP is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * RAP is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with RAP. If not, see <http://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
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
