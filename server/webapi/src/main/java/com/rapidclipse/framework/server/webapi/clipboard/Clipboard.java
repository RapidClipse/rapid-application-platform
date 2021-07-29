/*
 * Copyright (C) 2013-2021 by XDEV Software, All Rights Reserved.
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
 *     XDEV Software Corp. - initial API and implementation
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
