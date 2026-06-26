/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.charts;

import static java.util.Objects.requireNonNull;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;

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

	public Format format();

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

		public Builder format(Format format);

		public Builder property(String name, Object value);

		public Column build();

		public static class Default implements Builder
		{
			private Type         type;
			private String       label;
			private String       id;
			private Role         role;
			private String       pattern;
			private Format       format;
			private ObjectHelper properties;

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
			public Builder format(final Format format)
			{
				this.format = format;
				return this;
			}

			@Override
			public Builder property(final String name, final Object value)
			{
				if(this.properties == null)
				{
					this.properties = new ObjectHelper();
				}
				this.properties.put(name, value);
				return this;
			}

			@Override
			public Column build()
			{
				return new Column.Default(this.type, this.label, this.id, this.role, this.pattern, this.format,
					this.properties);
			}

		}

	}

	public static Column New(final Type type)
	{
		return new Default(type, null, null, null, null, null);
	}

	public static Column New(final Type type, final String id)
	{
		return new Default(type, id, id, null, null, null);
	}

	public static Column New(final Type type, final String id, final Format format)
	{
		return new Default(type, id, id, null, null, format);
	}

	public static Column New(final Type type, final String id, final String label)
	{
		return new Default(type, id, label, null, null, null);
	}

	public static Column New(final Type type, final String id, final String label, final Format format)
	{
		return new Default(type, id, label, null, null, format);
	}

	public static Column New(final Type type, final String id, final Role role)
	{
		return new Default(type, id, id, role, null, null);
	}

	public static Column New(final Type type, final String id, final String label, final Role role)
	{
		return new Default(type, id, label, role, null, null);
	}

	public static Column ToolTip(final String id)
	{
		return new Default(Type.STRING, id, null, Role.TOOLTIP, null, null);
	}

	public static Column
		New(final Type type, final String id, final String label, final Role role, final String pattern)
	{
		return new Default(type, id, label, role, pattern, null);
	}

	public static Column
		New(
			final Type type,
			final String id,
			final String label,
			final Role role,
			final String pattern,
			final Format format)
	{
		return new Default(type, id, label, role, pattern, format);
	}

	public static class Default implements Column
	{
		private final Type         type;
		private final String       id;
		private final String       label;
		private final Role         role;
		private final String       pattern;
		private final Format       format;
		private final ObjectHelper properties;

		Default(
			final Type type,
			final String id,
			final String label,
			final Role role,
			final String pattern,
			final Format format)
		{
			this(type, id, label, role, pattern, format, null);
		}

		Default(
			final Type type,
			final String id,
			final String label,
			final Role role,
			final String pattern,
			final Format format,
			final ObjectHelper properties)
		{
			super();

			this.type       = requireNonNull(type, "Null type is not allowed");
			this.id         = id;
			this.label      = label;
			this.role       = role;
			this.pattern    = pattern;
			this.format     = format;
			this.properties = properties;
		}

		@Override
		public Type type()
		{
			return this.type;
		}

		@Override
		public String id()
		{
			return this.id;
		}

		@Override
		public String label()
		{
			return this.label;
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
		public Format format()
		{
			return this.format;
		}

		@Override
		public String js()
		{
			if(this.role != null || !StringUtils.isEmpty(this.pattern))
			{
				final ObjectHelper obj = new ObjectHelper();
				obj.put("type", this.type);
				obj.putIfNotNull("id", this.id);
				obj.putIfNotNull("label", this.label);
				obj.putIfNotNull("role", this.role);
				obj.putIfNotNull("pattern", this.pattern);
				obj.putIfNotNull("p", this.properties);
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
