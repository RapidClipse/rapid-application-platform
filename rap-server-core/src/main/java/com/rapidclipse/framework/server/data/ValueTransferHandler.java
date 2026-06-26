/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.data;

/**
 * @author XDEV Software
 *
 */
public interface ValueTransferHandler
{
	public boolean handlesPut(Object value);
	
	public Object put(Object value);
	
	public boolean handlesGet(Object value);
	
	public Object get(Object value);
}
