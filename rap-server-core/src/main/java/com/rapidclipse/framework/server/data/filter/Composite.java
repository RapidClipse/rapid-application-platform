/*
 * Copyright (C) 2013-2023 by XDEV Software, All Rights Reserved.
 *
 * This file is part of the RapidClipse Application Platform (RAP).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software - initial API and implementation
 */
package com.rapidclipse.framework.server.data.filter;

import static com.rapidclipse.framework.server.Rap.notEmpty;
import static java.util.Arrays.asList;
import static java.util.Collections.unmodifiableList;

import java.util.List;


/**
 * @author XDEV Software
 *
 */
public interface Composite extends Filter
{
	public static enum Connector
	{
		AND,
		OR
	}

	public Connector connector();

	public List<Filter> filters();

	public static Composite New(final Connector connector, final Filter... filters)
	{
		return New(connector, asList(filters));
	}

	public static Composite New(final Connector connector, final List<Filter> filters)
	{
		return new Default(connector, filters);
	}

	public static class Default implements Composite
	{
		private final Connector    connector;
		private final List<Filter> filters;

		protected Default(final Connector connector, final List<Filter> filters)
		{
			super();

			this.connector = connector;
			this.filters   = unmodifiableList(notEmpty(filters));
		}

		@Override
		public Connector connector()
		{
			return this.connector;
		}

		@Override
		public List<Filter> filters()
		{
			return this.filters;
		}
	}
}
