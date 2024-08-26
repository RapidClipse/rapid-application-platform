/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.data.renderer;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;

import com.vaadin.flow.data.renderer.BasicRenderer;
import com.vaadin.flow.function.SerializableSupplier;
import com.vaadin.flow.function.ValueProvider;

/**
 * A template renderer for presenting date values.
 *
 * @author XDEV Software
 * 
 * @param <SOURCE>
 *        the type of the input item, from which the {@link Date} is extracted
 */
public class DateRenderer<SOURCE> extends BasicRenderer<SOURCE, Date>
{
	private SerializableSupplier<DateFormat> formatter;
	private String                           nullRepresentation;

	/**
	 * Creates a new DateRenderer.
	 * <p>
	 * The renderer is configured with the default date format and an empty string
	 * as its null representation.
	 * </p>
	 * 
	 * @param valueProvider
	 *        the callback to provide a {@link Date} to the renderer, not
	 *        <code>null</code>
	 * 
	 * @see DateFormat#getDateInstance()
	 */
	public DateRenderer(
		final ValueProvider<SOURCE, Date> valueProvider)
	{
		this(valueProvider, () -> DateFormat.getDateInstance(), "");
	}

	/**
	 * Creates a new DateRenderer.
	 * <p>
	 * The renderer is configured to render with the given string format, with an
	 * empty string as its null representation.
	 * </p>
	 * 
	 * @param valueProvider
	 *        the callback to provide a {@link Date} to the renderer, not
	 *        <code>null</code>
	 * @param formatPattern
	 *        the format pattern to format the date with, not <code>null</code>
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
	 * </p>
	 * 
	 * @param valueProvider
	 *        the callback to provide a {@link Date} to the renderer, not
	 *        <code>null</code>
	 * @param formatPattern
	 *        the format pattern to format the date with, not <code>null</code>
	 * @param locale
	 *        the locale to use, not <code>null</code>
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
	 * </p>
	 * 
	 * @param valueProvider
	 *        the callback to provide a {@link Date} to the renderer, not
	 *        <code>null</code>
	 * @param formatPattern
	 *        the format pattern to format the date with, not <code>null</code>
	 * @param locale
	 *        the locale to use, not <code>null</code>
	 * @param nullRepresentation
	 *        the textual representation of the <code>null</code> value
	 */
	public DateRenderer(
		final ValueProvider<SOURCE, Date> valueProvider,
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

		this.formatter = () -> new SimpleDateFormat(formatPattern, locale);
		this.nullRepresentation = nullRepresentation;
	}

	/**
	 * Creates a new DateRenderer.
	 * <p>
	 * The renderer is configured to render with the given formatter, with an empty
	 * string as its null representation.
	 * </p>
	 * 
	 * @param valueProvider
	 *        the callback to provide a {@link Date} to the renderer, not
	 *        <code>null</code>
	 * @param format
	 *        the format to use, not <code>null</code>
	 * 
	 * @deprecated Via this constructor renderer is not serializable, use
	 *             {@link DateRenderer(ValueProvider, SerializableSupplier)}
	 *             instead.
	 * 
	 * @see https://github.com/vaadin/flow-components/issues/2659
	 */
	@Deprecated
	public DateRenderer(
		final ValueProvider<SOURCE, Date> valueProvider,
		final DateFormat format)
	{
		this(valueProvider, () -> format, "");
	}

	/**
	 * Creates a new DateRenderer.
	 * <p>
	 * The renderer is configured to render with the given formatter, with the empty
	 * string as its null representation.
	 * </p>
	 * 
	 * @param valueProvider
	 *        the callback to provide a {@link Date} to the renderer, not
	 *        <code>null</code>
	 * @param formatter
	 *        the format to use, not <code>null</code>
	 */
	public DateRenderer(
		ValueProvider<SOURCE, Date> valueProvider,
		SerializableSupplier<DateFormat> formatter)
	{
		this(valueProvider, formatter, "");
	}

	/**
	 * Creates a new DateRenderer.
	 * <p>
	 * The renderer is configured to render with the given format.
	 * </p>
	 * 
	 * @param valueProvider
	 *        the callback to provide a {@link Date} to the renderer, not
	 *        <code>null</code>
	 * @param format
	 *        the format to use, not <code>null</code>
	 * @param nullRepresentation
	 *        the textual representation of the <code>null</code> value
	 * 
	 * @deprecated Via this constructor renderer is not serializable, use
	 *             {@link DateRenderer(ValueProvider, SerializableSupplier, String)}
	 *             instead.
	 * 
	 * @see https://github.com/vaadin/flow-components/issues/2659
	 */
	@Deprecated
	public DateRenderer(
		final ValueProvider<SOURCE, Date> valueProvider,
		final DateFormat format,
		final String nullRepresentation)
	{
		this(valueProvider, () -> format, nullRepresentation);
	}

	/**
	 * Creates a new DateRenderer.
	 * <p>
	 * The renderer is configured to render with the given formatter.
	 * </p>
	 * 
	 * @param valueProvider
	 *        the callback to provide a {@link Date} to the renderer, not
	 *        <code>null</code>
	 * @param formatter
	 *        the formatter to use, not <code>null</code>
	 * @param nullRepresentation
	 *        the textual representation of the <code>null</code> value
	 */
	public DateRenderer(
		ValueProvider<SOURCE, Date> valueProvider,
		SerializableSupplier<DateFormat> formatter,
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
	protected String getFormattedValue(final Date date)
	{
		try
		{
			return date == null ? this.nullRepresentation : this.formatter.get().format(date);
		}
		catch (final Exception e)
		{
			throw new IllegalStateException("Could not format input date '" + date + "' using formatter '" + this.formatter + "'", e);
		}
	}
}
