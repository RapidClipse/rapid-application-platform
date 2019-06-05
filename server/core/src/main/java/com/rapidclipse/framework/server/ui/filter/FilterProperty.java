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

import static java.util.Objects.requireNonNull;

import java.io.Serializable;


public interface FilterProperty<T> extends Serializable
{
	public Object identifier();

	public Class<T> type();

	public String caption();

	public static <T> FilterProperty<T> New(
		final Object identifier,
		final Class<T> type,
		final String caption)
	{
		return new Implementation<>(identifier, type, caption);
	}

	public static class Implementation<T> implements FilterProperty<T>
	{
		private final Object   identifier;
		private final Class<T> type;
		private final String   caption;

		public Implementation(final Object identifier, final Class<T> type, final String caption)
		{
			super();

			this.identifier = requireNonNull(identifier);
			this.type       = requireNonNull(type);
			this.caption    = requireNonNull(caption);
		}

		@Override
		public Object identifier()
		{
			return this.identifier;
		}

		@Override
		public Class<T> type()
		{
			return this.type;
		}

		@Override
		public String caption()
		{
			return this.caption;
		}
	}
}
