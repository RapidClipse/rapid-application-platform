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

package com.rapidclipse.framework.server.reports.grid;

import org.apache.commons.lang3.StringUtils;

import com.rapidclipse.framework.server.resources.CaptionUtils;
import com.vaadin.flow.component.grid.ColumnTextAlign;
import com.vaadin.flow.component.grid.Grid.Column;


/**
 *
 * @author XDEV Software
 * @since 10.01.00
 */
public class ColumnConfiguration<T>
{
	private final Column<T> gridColumn;
	private boolean         visible = true;
	private String          header;
	private ColumnTextAlign columnAlignment;
	private Integer         columnWidth;

	ColumnConfiguration(final Column<T> gridColumn)
	{
		super();

		this.gridColumn      = gridColumn;
		this.header          = resolveHeader(gridColumn);
		this.columnAlignment = gridColumn.getTextAlign();
	}

	static String resolveHeader(final Column<?> gridColumn)
	{
		final String key = gridColumn.getKey();
		if(key == null)
		{
			return "";
		}

		final Class<?> beanType = gridColumn.getGrid().getBeanType();
		if(beanType != null)
		{
			try
			{
				final String caption = CaptionUtils.resolveCaption(beanType, gridColumn.getKey());
				if(!StringUtils.isEmpty(caption))
				{
					return caption;
				}
			}
			catch(final Exception e)
			{
				// swallow
			}
		}

		return key;
	}

	Column<T> getGridColumn()
	{
		return this.gridColumn;
	}

	public boolean isVisible()
	{
		return this.visible;
	}

	public ColumnConfiguration<T> setVisible(final boolean visible)
	{
		this.visible = visible;
		return this;
	}

	public String getHeader()
	{
		return this.header;
	}

	public ColumnConfiguration<T> setHeader(final String header)
	{
		this.header = header;
		return this;
	}

	public ColumnTextAlign getColumnAlignment()
	{
		return this.columnAlignment;
	}

	public ColumnConfiguration<T> setColumnAlignment(final ColumnTextAlign columnAlignment)
	{
		this.columnAlignment = columnAlignment;
		return this;
	}

	public Integer getColumnWidth()
	{
		return this.columnWidth;
	}

	public ColumnConfiguration<T> setColumnWidth(final Integer columnWidth)
	{
		this.columnWidth = columnWidth;
		return this;
	}
}
