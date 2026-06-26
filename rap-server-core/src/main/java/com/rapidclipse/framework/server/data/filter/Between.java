/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.data.filter;

/**
 * @author XDEV Software
 *
 */
public interface Between extends Filter
{
	public Object identifier();

	public Comparable<?> start();

	public Comparable<?> end();
	
	public static Between New(
		final Object identifier,
		final Comparable<?> start,
		final Comparable<?> end)
	{
		return new Default(identifier, start, end);
	}

	public static class Default implements Between
	{
		private final Object        identifier;
		private final Comparable<?> start;
		private final Comparable<?> end;

		protected Default(
			final Object identifier,
			final Comparable<?> start,
			final Comparable<?> end)
		{
			super();

			this.identifier = identifier;
			this.start      = start;
			this.end        = end;
		}

		@Override
		public Object identifier()
		{
			return this.identifier;
		}

		@Override
		public Comparable<?> start()
		{
			return this.start;
		}

		@Override
		public Comparable<?> end()
		{
			return this.end;
		}
	}
}
