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
package com.rapidclipse.framework.server.mobilekit.compass;

import com.vaadin.flow.shared.Registration;


/**
 * @author XDEV Software
 *
 */
public interface HeadingWatch extends Registration
{
	/**
	 * @return the heading
	 */
	public Heading getHeading();
	
	/**
	 * Stops watching for changes to the device's heading
	 */
	@Override
	public void remove();
}
