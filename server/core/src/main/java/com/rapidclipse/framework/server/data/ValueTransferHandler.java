/*-
 * ---
 * Rapid Application Platform / Server / Core
 * --
 * Copyright (C) 2013 - 2019 XDEV Software Corp.
 * --
 * This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * which accompanies this distribution, and is available at
 * https://www.eclipse.org/legal/epl-2.0/
 * 
 * SPDX-License-Identifier: EPL-2.0
 * 
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 * ---
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
