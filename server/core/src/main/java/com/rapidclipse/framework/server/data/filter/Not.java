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
package com.rapidclipse.framework.server.data.filter;

/**
 * @author XDEV Software
 *
 */
public interface Not extends Filter
{
	public Filter filter();
	
	public static class Implementation implements Not
	{
		private final Filter filter;
		
		public Implementation(final Filter filter)
		{
			super();
			this.filter = filter;
		}
		
		@Override
		public Filter filter()
		{
			return this.filter;
		}
	}
}
