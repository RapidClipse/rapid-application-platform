/*
 * Copyright (C) 2013-2022 by XDEV Software, All Rights Reserved.
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
package com.rapidclipse.framework.server.data.renderer;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;
import java.util.Objects;

import com.vaadin.flow.data.renderer.BasicRenderer;
import com.vaadin.flow.function.ValueProvider;


/**
 * A template renderer for presenting date values.
 *
 * @author XDEV Software
 *
 * @param <SOURCE>
 *            the type of the input item, from which the {@link Date} is
 *            extracted
 */
public class DateRenderer<SOURCE> extends BasicRenderer<SOURCE, Date>
{
	private DateFormat format;
	private String     nullRepresentation;

	/**
	 * Creates a new DateRenderer.
	 * <p>
	 * The renderer is configured with the default date format
	 * and an empty string as its null representation.
	 *
	 * @param valueProvider
	 *            the callback to provide a {@link Date} to the renderer,
	 *            not <code>null</code>
	 *
	 * @see DateFormat#getDateInstance()
	 */
	public DateRenderer(final ValueProvider<SOURCE, Date> valueProvider)
	{
		this(valueProvider, DateFormat.getDateInstance(), "");
	}

	/**
	 * Creates a new DateRenderer.
	 * <p>
	 * The renderer is configured to render with the given string format, with
	 * an empty string as its null representation.
	 *
	 * @param valueProvider
	 *            the callback to provide a {@link Date} to the renderer,
	 *            not <code>null</code>
	 *
	 * @param formatPattern
	 *            the format pattern to format the date with, not
	 *            <code>null</code>
	 */
	public DateRenderer(
		final ValueProvider<SOURCE, Date> valueProvider,
		final String formatPattern)
	{
		this(valueProvider, formatPattern, Locale.getDefault());
	}

	/**
	 * Creates a new DateRenderer.
	 * <p>
	 * The renderer is configured to render with the given string format, as
	 * displayed in the given locale, with an empty string as its null
	 * representation.
	 *
	 * @param valueProvider
	 *            the callback to provide a {@link Date} to the renderer,
	 *            not <code>null</code>
	 * @param formatPattern
	 *            the format pattern to format the date with, not
	 *            <code>null</code>
	 * @param locale
	 *            the locale to use, not <code>null</code>
	 */
	public DateRenderer(
		final ValueProvider<SOURCE, Date> valueProvider,
		final String formatPattern,
		final Locale locale)
	{
		this(valueProvider, formatPattern, locale, "");
	}

	/**
	 * Creates a new DateRenderer.
	 * <p>
	 * The renderer is configured to render with the given string format, as
	 * displayed in the given locale.
	 *
	 * @param valueProvider
	 *            the callback to provide a {@link Date} to the renderer,
	 *            not <code>null</code>
	 * @param formatPattern
	 *            the format pattern to format the date with, not
	 *            <code>null</code>
	 * @param locale
	 *            the locale to use, not <code>null</code>
	 * @param nullRepresentation
	 *            the textual representation of the <code>null</code> value
	 */
	public DateRenderer(
		final ValueProvider<SOURCE, Date> valueProvider,
		final String formatPattern,
		final Locale locale,
		final String nullRepresentation)
	{
		super(valueProvider);

		this.format             = new SimpleDateFormat(
			Objects.requireNonNull(formatPattern, "format pattern may not be null"),
			Objects.requireNonNull(locale, "locale may not be null"));
		this.nullRepresentation = nullRepresentation;
	}

	/**
	 * Creates a new DateRenderer.
	 * <p>
	 * The renderer is configured to render with the given formatter, with an
	 * empty string as its null representation.
	 *
	 * @param valueProvider
	 *            the callback to provide a {@link Date} to the renderer,
	 *            not <code>null</code>
	 * @param format
	 *            the format to use, not <code>null</code>
	 */
	public DateRenderer(
		final ValueProvider<SOURCE, Date> valueProvider,
		final DateFormat format)
	{
		this(valueProvider, format, "");
	}

	/**
	 * Creates a new DateRenderer.
	 * <p>
	 * The renderer is configured to render with the given format.
	 *
	 * @param valueProvider
	 *            the callback to provide a {@link Date} to the renderer,
	 *            not <code>null</code>
	 * @param format
	 *            the format to use, not <code>null</code>
	 * @param nullRepresentation
	 *            the textual representation of the <code>null</code> value
	 *
	 */
	public DateRenderer(
		final ValueProvider<SOURCE, Date> valueProvider,
		final DateFormat format,
		final String nullRepresentation)
	{
		super(valueProvider);

		this.format = Objects.requireNonNull(format, "format may not be null");
	}

	@Override
	protected String getFormattedValue(final Date date)
	{
		try
		{
			return date == null ? this.nullRepresentation : this.format.format(date);
		}
		catch(final Exception e)
		{
			throw new IllegalStateException("Could not format input date '"
				+ date + "' using format '" + this.format + "'", e);
		}
	}
}
