/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
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
