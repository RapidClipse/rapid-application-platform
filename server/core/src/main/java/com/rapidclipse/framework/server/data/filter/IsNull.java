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

package com.rapidclipse.framework.server.data.filter;

/**
 * @author XDEV Software
 *
 */
public interface IsNull extends Filter
{
	public Object identifier();
	
	public static class Implementation implements IsNull
	{
		private final Object identifier;
		
		public Implementation(final Object identifier)
		{
			super();
			this.identifier = identifier;
		}
		
		@Override
		public Object identifier()
		{
			return this.identifier;
		}
	}
}