/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 *
 * For further information see
 * <http://www.rapidclipse.com/en/legal/license/license.html>.
 */

package com.rapidclipse.framework.server.charts.data;

import java.io.Serializable;


/**
 *
 * @author XDEV Software (SS)
 * @since 4.0
 */
public class Column implements Serializable
{
	private String   id;
	private String   label;
	private String   type;
	private DataRole datarole;
	
	private Column(final String id, final String label, final ColumnType type)
	{
		this.id    = id;
		this.label = label;
		this.type  = type.text();
	}
	
	private Column(final String id, final String label, final ColumnType type, final DataRoleType role)
	{
		this.id       = id;
		this.label    = label;
		this.type     = type.text();
		this.datarole = new DataRole(role);
	}
	
	public static Column show(final String label, final ColumnType type)
	{
		return Column.create(label, label, type, true);
	}
	
	public static Column create(final String id, final String label, final ColumnType type)
	{
		return Column.create(id, label, type, true);
	}

	// Wird immer mit visible=true aufgerufen
	public static Column create(
		final String id,
		final String label,
		final ColumnType type,
		final boolean visible)
	{
		if(visible)
		{
			return new Column(id, label, type);
		}
		else
		{
			return new Column(id, "hidden", type);
		}
	}
	
	public static Column stringColumn(final String id, final String label)
	{
		return new Column(id, label, ColumnType.STRING);
	}
	
	public static Column numberColumn(final String id, final String label)
	{
		return new Column(id, label, ColumnType.NUMBER);
	}
	
	public static Column dateTimeColumn(final String id, final String label)
	{
		return new Column(id, label, ColumnType.DATETIME);
	}
	
	public static Column dateColumn(final String id, final String label)
	{
		return new Column(id, label, ColumnType.DATE);
	}
	
	public static Column
		dataRoleColumn(final String id, final String label, final ColumnType type, final DataRoleType role)
	{
		return new Column(id, label, type, role);
	}
	
	private static String caption = "caption";
	
	public static Column captionColumnAsString(final String label)
	{
		return new Column(Column.caption, label, ColumnType.STRING);
	}
	
	public static Column captionColumnAsNumber(final String label)
	{
		return new Column(Column.caption, label, ColumnType.NUMBER);
	}
	
	public static Column captionColumnAsDate(final String label)
	{
		return new Column(Column.caption, label, ColumnType.DATE);
	}
	
	public static Column captionColumnAsDateTime(final String label)
	{
		return new Column(Column.caption, label, ColumnType.DATETIME);
	}
	
	public String getId()
	{
		return this.id;
	}
	
	public void setId(final String id)
	{
		this.id = id;
	}
	
	public String getLabel()
	{
		return this.label;
	}
	
	public void setLabel(final String label)
	{
		this.label = label;
	}
	
	public String getType()
	{
		return this.type;
	}
	
	public void setType(final String type)
	{
		this.type = type;
	}
	
	public DataRole getDatarole()
	{
		return this.datarole;
	}
	
	public void setDatarole(final DataRole p)
	{
		this.datarole = p;
	}

	public String jsPrint()
	{
		final StringBuilder print = new StringBuilder();

		if(this.datarole != null)
		{
			print.append("{type:'" + this.type + "', role:'" + this.datarole.getRole() + "'}");
		}
		else
		{
			print.append("'" + this.type + "', '" + this.label + "'");
		}
		return print.toString();
	}
	
}
