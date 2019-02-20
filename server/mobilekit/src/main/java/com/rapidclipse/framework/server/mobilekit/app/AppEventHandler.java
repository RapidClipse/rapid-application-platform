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

import java.util.EventListener;


/**
 * Handler for events fired by {@link AppService}.
 *
 * @author XDEV Software
 *
 */
@FunctionalInterface
public interface AppEventHandler extends EventListener
{
	/**
	 * Called when an app event happened.
	 * <p>
	 * Propagation to other listeners can be prevented by calling
	 * {@link AppEvent#consume()}.
	 *
	 * @param event
	 */
	public void handleAppEvent(AppEvent event);
}
