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

package com.rapidclipse.framework.server.mobilekit.sms;

/**
 * @author XDEV Software
 *
 */
public class SmsOptions
{
	public static SmsOptions defaults()
	{
		return new SmsOptions();
	}

	private boolean replaceLineBreaks = false;
	private boolean androidNativeApp  = false;
	
	public SmsOptions()
	{
	}
	
	/**
	 * <code>true</code> to replace \n by a new line, <code>false</code> by
	 * default
	 */
	public SmsOptions replaceLineBreaks(final boolean replaceLineBreaks)
	{
		this.replaceLineBreaks = replaceLineBreaks;
		return this;
	}
	
	/**
	 * @return the replaceLineBreaks setting
	 */
	public boolean isReplaceLineBreaks()
	{
		return this.replaceLineBreaks;
	}
	
	/**
	 * <code>true</code> if the SMS should be sent with the native android SMS
	 * messaging
	 */
	public SmsOptions androidNativeApp(final boolean androidNativeApp)
	{
		this.androidNativeApp = androidNativeApp;
		return this;
	}
	
	/**
	 * @return the androidNativeApp setting
	 */
	public boolean isAndroidNativeApp()
	{
		return this.androidNativeApp;
	}
}
