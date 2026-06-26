/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.webapi.clipboard;

import com.vaadin.flow.component.UI;
import com.vaadin.flow.function.SerializableConsumer;


/**
 * With this static utilities you can access the current devices clipboard. For writing to work the user first has to
 * give permission.
 *
 * @author XDEV Software
 * @since 10.02.00
 */
public final class Clipboard
{
	/**
	 * Read the current content of the devices clipboard. When the content is received, the onTextReceived callback is
	 * triggered.
	 *
	 * @param callback
	 *            The callback that consumes the text received from the devices clipboard.
	 */
	public static void readText(final SerializableConsumer<String> callback)
	{
		UI.getCurrent().getPage().executeJs("return navigator.clipboard.readText()").then(String.class, callback);
	}

	/**
	 * Write a string into the devices clipboard. The user first has to give permission for this to work.
	 *
	 * @param text
	 *            The text that should be written to the clipboard.
	 */
	public static void writeText(final String text)
	{
		UI.getCurrent().getPage().executeJs("navigator.clipboard.writeText($0)", text);
	}

	private Clipboard()
	{
		throw new Error();
	}
}
