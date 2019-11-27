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

import static java.util.Objects.requireNonNull;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;

import com.rapidclipse.framework.server.util.JavaScriptable;

import elemental.json.Json;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
public interface Column extends Serializable, JavaScriptable
{
	public static enum Type implements JavaScriptable
	{
		STRING("string"),
		NUMBER("number"),
		BOOLEAN("boolean"),
		DATE("date"),
		DATE_TIME("datetime"),
		TIME_OF_DAY("timeofday");

		private final String js;

		private Type(final String js)
		{
			this.js = Json.create(js).toJson();
		}

		@Override
		public String js()
		{
			return this.js;
		}
	}

	public static enum Role implements JavaScriptable
	{
		ANNOTATION("annotation"),
		ANNOTATION_TEXT("annotationText"),
		CERTAINTY("certainty"),
		EMPHASIS("emphasis"),
		INTERVAL("interval"),
		SCOPE("scope"),
		TOOLTIP("tooltip"),
		DOMAIN("domain"),
		DATA("data"),
		STYLE("style");

		private final String js;

		private Role(final String js)
		{
			this.js = Json.create(js).toJson();
		}

		@Override
		public String js()
		{
			return this.js;
		}
	}

	public Type type();

	public String label();

	public String id();

	public Role role();

	public String pattern();

	public static Builder Builder()
	{
		return new Builder.Default();
	}

	public static interface Builder
	{
		public Builder type(Type type);

		public Builder label(String label);

		public Builder id(String id);

		public Builder role(Role role);

		public Builder pattern(String pattern);

		public Column build();

		public static class Default implements Builder
		{
			private Type   type;
			private String label;
			private String id;
			private Role   role;
			private String pattern;

			Default()
			{
				super();
			}

			@Override
			public Builder type(final Type type)
			{
				this.type = type;
				return this;
			}

			@Override
			public Builder label(final String label)
			{
				this.label = label;
				return this;
			}

			@Override
			public Builder id(final String id)
			{
				this.id = id;
				return this;
			}

			@Override
			public Builder role(final Role role)
			{
				this.role = role;
				return this;
			}

			@Override
			public Builder pattern(final String pattern)
			{
				this.pattern = pattern;
				return this;
			}

			@Override
			public Column build()
			{
				return Column.New(this.type, this.label, this.id, this.role, this.pattern);
			}

		}

	}

	public static Column New(final Type type)
	{
		return new Default(type, null, null, null, null);
	}

	public static Column New(final Type type, final String label)
	{
		return new Default(type, label, label, null, null);
	}

	public static Column New(final Type type, final String label, final String id)
	{
		return new Default(type, label, id, null, null);
	}

	public static Column New(final Type type, final String label, final String id, final Role role)
	{
		return new Default(type, label, id, role, null);
	}

	public static Column
		New(final Type type, final String label, final String id, final Role role, final String pattern)
	{
		return new Default(type, label, id, role, pattern);
	}

	public static Column New(final Type type, final Role role)
	{
		return new Default(type, null, null, role, null);
	}

	public static class Default implements Column
	{
		private final Type   type;
		private final String label;
		private final String id;
		private final Role   role;
		private final String pattern;

		Default(final Type type, final String label, final String id, final Role role, final String pattern)
		{
			super();

			this.type    = requireNonNull(type, "Null type is not allowed");
			this.label   = label;
			this.id      = id;
			this.role    = role;
			this.pattern = pattern;
		}

		@Override
		public Type type()
		{
			return this.type;
		}

		@Override
		public String label()
		{
			return this.label;
		}

		@Override
		public String id()
		{
			return this.id;
		}

		@Override
		public Role role()
		{
			return this.role;
		}

		@Override
		public String pattern()
		{
			return this.pattern;
		}

		@Override
		public String js()
		{
			if(this.role != null || !StringUtils.isEmpty(this.pattern))
			{
				final ObjectHelper obj = new ObjectHelper();
				obj.put("type", this.type);
				obj.putIfNotNull("label", this.label);
				obj.putIfNotNull("id", this.id);
				obj.putIfNotNull("role", this.role);
				obj.putIfNotNull("pattern", this.pattern);
				return obj.js();
			}

			final List<String> params = new ArrayList<>();
			params.add(this.type.js());
			if(!StringUtils.isEmpty(this.id))
			{
				if(!StringUtils.isEmpty(this.label))
				{
					params.add(Json.create(this.label).toJson());
				}
				else
				{
					params.add("null");
				}

				params.add(Json.create(this.id).toJson());
			}
			else if(!StringUtils.isEmpty(this.label))
			{
				params.add(Json.create(this.label).toJson());
			}

			return params.stream().collect(Collectors.joining(","));
		}

	}

}
