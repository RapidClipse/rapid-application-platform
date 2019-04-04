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
package com.rapidclipse.framework.server.mobilekit.app;

import java.util.EventObject;


/**
 * An event fired by the {@link AppService}.
 *
 * @author XDEV Software
 *
 */
public class AppEvent extends EventObject
{
	private boolean consumed = false;
	
	public AppEvent(final AppService source)
	{
		super(source);
	}
	
	@Override
	public AppService getSource()
	{
		return (AppService)super.getSource();
	}
	
	/**
	 * Consumes this event so that it will not be processed in the default
	 * manner by the source which originated it.
	 */
	public void consume()
	{
		this.consumed = true;
	}
	
	/**
	 * Returns whether or not this event has been consumed.
	 *
	 * @see #consume
	 */
	public boolean isConsumed()
	{
		return this.consumed;
	}
}
