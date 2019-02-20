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

package com.rapidclipse.framework.server.mobilekit.geolocation;

import com.vaadin.flow.shared.Registration;


/**
 * Callback object for
 * {@link GeolocationService#watchPosition(GeolocationOptions, java.util.function.Consumer, java.util.function.Consumer)}
 *
 * @author XDEV Software
 *
 */
public interface PositionWatch extends Registration
{
	/**
	 * @return the position
	 */
	public Position getPosition();
	
	/**
	 * Stops watching for changes to the device's location
	 */
	@Override
	public void remove();
}
