/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
 *
 * This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * which accompanies this distribution, and is available at
 * https://www.eclipse.org/legal/epl-2.0/
 *
 * SPDX-License-Identifier: EPL-2.0
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 */
package com.rapidclipse.framework.server.mobilekit.vibration;

import java.util.stream.Collectors;
import java.util.stream.IntStream;

import com.rapidclipse.framework.server.mobilekit.MobileComponent;
import com.vaadin.flow.component.Tag;
import com.vaadin.flow.component.dependency.HtmlImport;


/**
 * @author XDEV Software
 *
 */
@Tag("mobilekit-vibration")
@HtmlImport("vibration.html")
public class VibrationComponent extends MobileComponent implements VibrationService
{
	public VibrationComponent()
	{
		super();
	}
	
	@Override
	public void vibrate(final int... pattern)
	{
		final String time = pattern.length == 1 ? String.valueOf(pattern[0])
			: IntStream.of(pattern).mapToObj(String::valueOf).collect(Collectors.joining(",", "[", "]"));
		getElement().callFunction("vibrate", time);
	}
}
