/*
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
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

import java.time.LocalTime;
import java.time.format.DateTimeFormatter;
import java.time.format.FormatStyle;
import java.util.Locale;

import com.vaadin.flow.data.renderer.BasicRenderer;
import com.vaadin.flow.function.SerializableSupplier;
import com.vaadin.flow.function.ValueProvider;

/**
 * A template renderer for presenting time values.
 *
 * @author XDEV Software
 *
 * @param <SOURCE>
 *        the type of the input item, from which the {@link LocalTime} is
 *        extracted
 */
public class LocalTimeRenderer<SOURCE> extends BasicRenderer<SOURCE, LocalTime>
{
	private SerializableSupplier<DateTimeFormatter> formatter;
	private String                                  nullRepresentation;

	/**
	 * Creates a new LocalTimeRenderer.
	 * <p>
	 * The renderer is configured with the format style {@code FormatStyle.SHORT}
	 * and an empty string as its null representation.
	 * </p>
	 * 
	 * @param valueProvider
	 *        the callback to provide a {@link LocalTime} to the renderer, not
	 *        <code>null</code>
	 * 
	 * @see <a href=
	 *      "https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/time/format/FormatStyle.html#SHORT">
	 *      FormatStyle.SHORT</a>
	 */
	public LocalTimeRenderer(
		final ValueProvider<SOURCE, LocalTime> valueProvider)
	{
		this(valueProvider, () -> DateTimeFormatter.ofLocalizedTime(FormatStyle.SHORT), "");
	}

	/**
	 * Creates a new LocalTimeRenderer.
	 * <p>
	 * The renderer is configured to render with the given string format, with an
	 * empty string as its null representation.
	 * </p>
	 * 
	 * @param valueProvider
	 *        the callback to provide a {@link LocalTime} to the renderer, not
	 *        <code>null</code>
	 * @param formatPattern
	 *        the format pattern to format the time with, not <code>null</code>
	 * 
	 * @see <a href=
	 *      "https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/time/format/DateTimeFormatter.html#patterns">
	 *      Format Pattern Syntax</a>
	 */
	public LocalTimeRenderer(
		final ValueProvider<SOURCE, LocalTime> valueProvider,
		final String formatPattern)
	{
		this(valueProvider, formatPattern, Locale.getDefault());
	}

	/**
	 * Creates a new LocalTimeRenderer.
	 * <p>
	 * The renderer is configured to render with the given string format, as
	 * displayed in the given locale, with an empty string as its null
	 * representation.
	 * </p>
	 * 
	 * @param valueProvider
	 *        the callback to provide a {@link LocalTime} to the renderer, not
	 *        <code>null</code>
	 * @param formatPattern
	 *        the format pattern to format the time with, not <code>null</code>
	 * @param locale
	 *        the locale to use, not <code>null</code>
	 * 
	 * @see <a href=
	 *      "https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/time/format/DateTimeFormatter.html#patterns">
	 *      Format Pattern Syntax</a>
	 */
	public LocalTimeRenderer(
		final ValueProvider<SOURCE, LocalTime> valueProvider,
		final String formatPattern,
		final Locale locale)
	{
		this(valueProvider, formatPattern, locale, "");
	}

	/**
	 * Creates a new LocalTimeRenderer.
	 * <p>
	 * The renderer is configured to render with the given string format, as
	 * displayed in the given locale.
	 * </p>
	 * 
	 * @param valueProvider
	 *        the callback to provide a {@link LocalTime} to the renderer, not
	 *        <code>null</code>
	 * @param formatPattern
	 *        the format pattern to format the time with, not <code>null</code>
	 * @param locale
	 *        the locale to use, not <code>null</code>
	 * @param nullRepresentation
	 *        the textual representation of the <code>null</code> value
	 * 
	 * @see <a href=
	 *      "https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/time/format/DateTimeFormatter.html#patterns">
	 *      Format Pattern Syntax</a>
	 */
	public LocalTimeRenderer(
		final ValueProvider<SOURCE, LocalTime> valueProvider,
		final String formatPattern,
		final Locale locale,
		final String nullRepresentation)
	{
		super(valueProvider);

		if (formatPattern == null)
		{
			throw new IllegalArgumentException("format pattern may not be null");
		}

		if (locale == null)
		{
			throw new IllegalArgumentException("locale may not be null");
		}

		this.formatter = () -> DateTimeFormatter.ofPattern(formatPattern, locale);
		this.nullRepresentation = nullRepresentation;
	}

	/**
	 * Creates a new LocalTimeRenderer.
	 * <p>
	 * The renderer is configured to render with the given formatter, with an empty
	 * string as its null representation.
	 * </p>
	 * 
	 * @param valueProvider
	 *        the callback to provide a {@link LocalTime} to the renderer, not
	 *        <code>null</code>
	 * @param formatter
	 *        the formatter to use, not <code>null</code>
	 * 
	 * @deprecated Via this constructor renderer is not serializable, use
	 *             {@link LocalTimeRenderer(ValueProvider, SerializableSupplier)}
	 *             instead.
	 * 
	 * @see https://github.com/vaadin/flow-components/issues/2659
	 */
	@Deprecated
	public LocalTimeRenderer(
		final ValueProvider<SOURCE, LocalTime> valueProvider,
		final DateTimeFormatter formatter)
	{
		this(valueProvider, () -> formatter, "");
	}

	/**
	 * Creates a new LocalTimeRenderer.
	 * <p>
	 * The renderer is configured to render with the given formatter, with the empty
	 * string as its null representation.
	 * </p>
	 * 
	 * @param valueProvider
	 *        the callback to provide a {@link LocalTime} to the renderer, not
	 *        <code>null</code>
	 * @param formatter
	 *        the formatter to use, not <code>null</code>
	 */
	public LocalTimeRenderer(
		ValueProvider<SOURCE, LocalTime> valueProvider,
		SerializableSupplier<DateTimeFormatter> formatter)
	{
		this(valueProvider, formatter, "");
	}

	/**
	 * Creates a new LocalTimeRenderer.
	 * <p>
	 * The renderer is configured to render with the given formatter.
	 * </p>
	 * 
	 * @param valueProvider
	 *        the callback to provide a {@link LocalTime} to the renderer, not
	 *        <code>null</code>
	 * @param formatter
	 *        the formatter to use, not <code>null</code>
	 * @param nullRepresentation
	 *        the textual representation of the <code>null</code> value
	 * 
	 * @deprecated Via this constructor renderer is not serializable, use
	 *             {@link LocalTimeRenderer(ValueProvider, SerializableSupplier,
	 *             String)} instead.
	 * 
	 * @see https://github.com/vaadin/flow-components/issues/2659
	 */
	@Deprecated
	public LocalTimeRenderer(
		final ValueProvider<SOURCE, LocalTime> valueProvider,
		final DateTimeFormatter formatter,
		final String nullRepresentation)
	{
		this(valueProvider, () -> formatter, nullRepresentation);
	}

	/**
	 * Creates a new LocalTimeRenderer.
	 * <p>
	 * The renderer is configured to render with the given formatter.
	 * </p>
	 * 
	 * @param valueProvider
	 *        the callback to provide a {@link LocalTime} to the renderer, not
	 *        <code>null</code>
	 * @param formatter
	 *        the formatter to use, not <code>null</code>
	 * @param nullRepresentation
	 *        the textual representation of the <code>null</code> value
	 */
	public LocalTimeRenderer(
		ValueProvider<SOURCE, LocalTime> valueProvider,
		SerializableSupplier<DateTimeFormatter> formatter,
		String nullRepresentation)
	{
		super(valueProvider);

		if (formatter == null)
		{
			throw new IllegalArgumentException("formatter may not be null");
		}

		this.formatter = formatter;
		this.nullRepresentation = nullRepresentation;
	}

	@Override
	protected String getFormattedValue(final LocalTime time)
	{
		try
		{
			return time == null ? this.nullRepresentation : this.formatter.get().format(time);
		}
		catch (final Exception e)
		{
			throw new IllegalStateException("Could not format input time '" + time + "' using formatter '" + this.formatter + "'", e);
		}
	}
}
