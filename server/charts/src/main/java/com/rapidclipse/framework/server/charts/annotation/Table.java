
package com.rapidclipse.framework.server.charts.annotation;

import java.io.Serializable;

import com.rapidclipse.framework.server.util.JavaScriptable;

import elemental.json.Json;


/**
 * @author XDEV Software
 *
 */
public interface Table extends Serializable, JavaScriptable
{
	public static enum Column implements JavaScriptable
	{
		LABEL(0),
		TEXT(1);

		private final String js;

		private Column(final Integer index)
		{
			this.js = Json.create(index).toJson();
		}

		@Override
		public String js()
		{
			return this.js;
		}
	}

	public Boolean sortAscending();

	public Column sortColumn();

	public static Table New(final Boolean sortAscending, final Column sortColumn)
	{
		return new Default(sortAscending, sortColumn);
	}

	public static class Default implements Table
	{
		private final Boolean sortAscending;
		private final Column  sortColumn;

		Default(final Boolean sortAscending, final Column sortColumn)
		{
			super();

			this.sortAscending = sortAscending;
			this.sortColumn    = sortColumn;
		}

		@Override
		public Boolean sortAscending()
		{
			return this.sortAscending;
		}

		@Override
		public Column sortColumn()
		{
			return this.sortColumn;
		}

		@Override
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			obj.putIfNotNull("sortAscending", this.sortAscending);
			obj.putIfNotNull("sortColumn", this.sortColumn);
			return obj.js();
		}
	}
}
