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

package com.rapidclipse.framework.server.concurrent;

import com.rapidclipse.framework.server.util.ServiceLoader;


/**
 * @author XDEV Software
 *
 */
public abstract class AccessWrapper
{
	public static interface Participant
	{
		public void before();
		
		public void after();
	}
	
	protected void before()
	{
		ServiceLoader.forType(Participant.class).services().forEach(Participant::before);
	}
	
	protected void after()
	{
		ServiceLoader.forType(Participant.class).services().forEach(Participant::after);
	}
}
