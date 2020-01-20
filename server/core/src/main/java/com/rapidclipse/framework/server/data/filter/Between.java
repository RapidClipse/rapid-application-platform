/*
 * Copyright (C) 2013-2020 by XDEV Software, All Rights Reserved.
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
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
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
