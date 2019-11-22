/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
 *
 * This file is part of the RapidClipse Application Platform (RAP).
 *
 * RAP is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * RAP is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with RAP. If not, see <http://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 */

package com.rapidclipse.framework.server.charts;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

import com.rapidclipse.framework.server.util.JavaScriptable;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
public interface ChartModel extends Serializable, JavaScriptable
{
	public List<Column> columns();

	public List<List<Cell>> rows();

	public ChartModel addColumn(Column column);

	public ChartModel addColumns(final Column... columns);

	public ChartModel addColumns(final Iterable<Column> columns);

	public boolean removeColumn(Column column);

	public Column removeColumnAt(int index);

	public ChartModel addRow(Object... row);

	public ChartModel addRow(Iterable<? extends Object> row);

	public List<Cell> removeRowAt(int index);

	public ChartModel removeAllRows();

	public ChartModel removeAll();

	public String js(final String varName);

	public static ChartModel New()
	{
		return new Default();
	}

	public static class Default implements ChartModel
	{
		private final List<Column>     columns = new ArrayList<>();
		private final List<List<Cell>> rows    = new ArrayList<>();

		Default()
		{
			super();
		}

		@Override
		public List<Column> columns()
		{
			return Collections.unmodifiableList(this.columns);
		}

		@Override
		public List<List<Cell>> rows()
		{
			return Collections.unmodifiableList(this.rows);
		}

		@Override
		public ChartModel addColumn(final Column column)
		{
			this.columns.add(column);
			return this;
		}

		@Override
		public ChartModel addColumns(final Column... columns)
		{
			return addColumns(Arrays.asList(columns));
		}

		@Override
		public ChartModel addColumns(final Iterable<Column> columns)
		{
			columns.forEach(this.columns::add);
			return this;
		}

		@Override
		public boolean removeColumn(final Column column)
		{
			final boolean removed = this.columns.remove(column);
			return removed;
		}

		@Override
		public Column removeColumnAt(final int index)
		{
			final Column removed = this.columns.remove(index);
			return removed;
		}

		@Override
		public ChartModel addRow(final Object... row)
		{
			return addRow(Arrays.asList(row));
		}

		@Override
		public ChartModel addRow(final Iterable<? extends Object> row)
		{
			this.rows.add(
				Collections.unmodifiableList(
					StreamSupport.stream(row.spliterator(), false)
						.map(this::cell)
						.collect(Collectors.toList())));

			return this;
		}

		@Override
		public List<Cell> removeRowAt(final int index)
		{
			final List<Cell> removed = this.rows.remove(index);
			return removed;
		}

		@Override
		public ChartModel removeAllRows()
		{
			this.rows.clear();
			return this;
		}

		@Override
		public ChartModel removeAll()
		{
			this.columns.clear();
			this.rows.clear();
			return this;
		}

		private Cell cell(final Object value)
		{
			return value instanceof Cell
				? (Cell)value
				: Cell.New(value);
		}

		@Override
		public String js()
		{
			return this.js("data");
		}

		@Override
		public String js(final String varName)
		{
			final StringBuilder sb = new StringBuilder();
			sb.append("var ").append(varName).append(" = new google.visualization.DataTable();\n");

			this.columns.forEach(column -> sb.append(varName).append(".addColumn(").append(column.js()).append(");\n"));

			if(this.rows.size() == 1)
			{
				sb.append(varName).append(".addRow(")
					.append(this.rows.get(0).stream().map(Cell::js).collect(Collectors.joining(",", "[", "]")))
					.append(");");
			}
			else if(this.rows.size() > 1)
			{
				sb.append(varName).append(".addRows(")
					.append(this.rows.stream()
						.map(row -> row.stream().map(Cell::js).collect(Collectors.joining(",", "[", "]")))
						.collect(Collectors.joining(",\n", "[\n", "\n]")))
					.append(");\n");
			}

			return sb.toString();
		}

	}

}
