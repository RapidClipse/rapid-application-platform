package com.rapidclipse.framework.server.data.renderer;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.IOException;
import java.time.Instant;
import java.time.LocalTime;
import java.util.Date;
import java.util.Locale;

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
}
