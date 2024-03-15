package com.rapidclipse.framework.server.data.renderer;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.IOException;
import java.time.Instant;
import java.time.LocalTime;
import java.time.ZoneId;
import java.util.Date;
import java.util.Locale;
import java.io.ByteArrayOutputStream;
import java.io.ObjectOutputStream;
import java.text.DateFormat;
import java.time.format.DateTimeFormatter;
import java.time.format.FormatStyle;

import org.junit.jupiter.api.Test;

public class FormattedRendererTest
{
	@Test
	public void getDefaultFormattedDateValue() throws IOException
	{
		Locale.setDefault(Locale.GERMANY); // de_DE
		DateRenderer<?> renderer = new DateRenderer<>(value -> Date.from(Instant.now()));
		String formattedValue = renderer.getFormattedValue(Date.from(Instant.EPOCH));
		
		assertEquals("01.01.1970", formattedValue);
	}

	@Test
	public void getDefaultFormattedLocalTimeValue() throws IOException
	{
		Locale.setDefault(Locale.GERMANY); // de_DE
		LocalTimeRenderer<?> renderer = new LocalTimeRenderer<>(value -> LocalTime.now());
		String formattedValue = renderer.getFormattedValue(LocalTime.NOON);
		
		assertEquals("12:00", formattedValue);
	}

	@Test
	public void getDefaultFormattedInstantValue() throws IOException
	{
		Locale.setDefault(Locale.GERMANY); // de_DE
		InstantRenderer<?> renderer = new InstantRenderer<>((v) -> Instant.now());
		String formattedValue = renderer.getFormattedValue(Instant.EPOCH);
		
		assertEquals("1. Januar 1970, 01:00", formattedValue);
	}
	
	
	@Test
	public void dateRendererIsSerializable() throws IOException
	{
		DateRenderer<?> renderer = new DateRenderer<>(value -> Date.from(Instant.now()));
		new ObjectOutputStream(new ByteArrayOutputStream()).writeObject(renderer);

		renderer = new DateRenderer<>(value -> Date.from(Instant.now()), () -> DateFormat.getDateInstance());
		new ObjectOutputStream(new ByteArrayOutputStream()).writeObject(renderer);
	}

	@Test
	public void localTimeRendererIsSerializable() throws IOException
	{
		LocalTimeRenderer<?> renderer = new LocalTimeRenderer<>(value -> LocalTime.now());
		new ObjectOutputStream(new ByteArrayOutputStream()).writeObject(renderer);

		renderer = new LocalTimeRenderer<>(value -> LocalTime.now(), () -> DateTimeFormatter.ofLocalizedTime(FormatStyle.SHORT));
		new ObjectOutputStream(new ByteArrayOutputStream()).writeObject(renderer);
	}

	@Test
	public void instantRendererIsSerializable() throws IOException
	{
		InstantRenderer<?> renderer = new InstantRenderer<>((v) -> Instant.now());
		new ObjectOutputStream(new ByteArrayOutputStream()).writeObject(renderer);

		renderer = new InstantRenderer<>((v) -> Instant.now(), () -> DateTimeFormatter.ofLocalizedDateTime(FormatStyle.LONG, FormatStyle.SHORT).withZone(ZoneId.systemDefault()));
		new ObjectOutputStream(new ByteArrayOutputStream()).writeObject(renderer);
	}
}
