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

package com.rapidclipse.framework.server.ui.filter;

import java.io.Serializable;


/**
 * @author XDEV Software
 *
 */
public interface FilterSubjectFactory<S> extends Serializable
{
	public FilterSubject createFilterSubject(S source);

	public static abstract class Abstract<S> implements FilterSubjectFactory<S>
	{
		protected boolean isSearchable(final Class<?> type)
		{
			return String.class.equals(type);
		}

		protected boolean isFilterable(final Class<?> type)
		{
			return !type.isArray();
		}
	}
}
