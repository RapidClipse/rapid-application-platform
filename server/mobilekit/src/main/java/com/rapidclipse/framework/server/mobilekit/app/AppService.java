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

import com.rapidclipse.framework.server.mobilekit.MobileService;
import com.rapidclipse.framework.server.mobilekit.MobileServiceScriptConfiguration;
import com.rapidclipse.framework.server.mobilekit.Plugin;
import com.rapidclipse.framework.server.mobilekit.ServiceComponent;


/**
 * @author XDEV Software
 *
 */
@ServiceComponent(AppComponent.class)
@MobileServiceScriptConfiguration(plugins = @Plugin(name = "cordova-plugin-exitapp", spec = "1.0.0"))
public interface AppService extends MobileService
{
	public static AppService getCurrent()
	{
		return MobileService.getCurrent(AppService.class);
	}
	
	/**
	 * Closes the app.
	 * <p>
	 * Supported platforms:
	 * <ul>
	 * <li>Android</li>
	 * <li>Windows Phone 8</li>
	 * </ul>
	 *
	 */
	public void closeApp();
	
	/**
	 * Adds an handler for the pause event.
	 * <p>
	 * The pause event fires when the native platform puts the application into
	 * the background, typically when the user switches to a different
	 * application.
	 * <p>
	 * Supported platforms:
	 * <ul>
	 * <li>Android</li>
	 * <li>iOS</li>
	 * <li>Windows Phone 8</li>
	 * <li>Windows</li>
	 * </ul>
	 *
	 * @param handler
	 */
	public void addPauseHandler(AppEventHandler handler);
	
	/**
	 * Removes the previously registered pause handler.
	 *
	 * @param handler
	 * @see #addPauseHandler(AppEventHandler)
	 */
	public void removePauseHandler(AppEventHandler handler);
	
	/**
	 * Adds an handler for the resume event.
	 * <p>
	 * The resume event fires when the native platform pulls the application out
	 * from the background.
	 * <p>
	 * Supported platforms:
	 * <ul>
	 * <li>Android</li>
	 * <li>iOS</li>
	 * <li>Windows Phone 8</li>
	 * <li>Windows</li>
	 * </ul>
	 *
	 * @param handler
	 */
	public void addResumeHandler(AppEventHandler handler);
	
	/**
	 * Removes the previously registered resume handler.
	 *
	 * @param handler
	 * @see #addResumeHandler(AppEventHandler)
	 */
	public void removeResumeHandler(AppEventHandler handler);
	
	/**
	 * Adds an handler for the backbutton event.
	 * <p>
	 * The event fires when the user presses the back button. To override the
	 * default back-button behavior, register an event listener for the
	 * backbutton event.
	 * <p>
	 * Supported platforms:
	 * <ul>
	 * <li>Android</li>
	 * <li>Windows</li>
	 * </ul>
	 *
	 * @param handler
	 */
	public void addBackButtonHandler(AppEventHandler handler);
	
	/**
	 * Removes the previously registered backbutton handler.
	 *
	 * @param handler
	 * @see #addBackButtonHandler(AppEventHandler)
	 */
	public void removeBackButtonHandler(AppEventHandler handler);
	
	/**
	 * Adds an handler for the menubutton event.
	 * <p>
	 * The event fires when the user presses the menu button. Applying an event
	 * handler overrides the default menu button behavior.
	 * <p>
	 * Supported platforms:
	 * <ul>
	 * <li>Android</li>
	 * </ul>
	 *
	 * @param handler
	 */
	public void addMenuButtonHandler(AppEventHandler handler);
	
	/**
	 * Removes the previously registered menubutton handler.
	 *
	 * @param handler
	 * @see #addMenuButtonHandler(AppEventHandler)
	 */
	public void removeMenuButtonHandler(AppEventHandler handler);
	
	/**
	 * Adds an handler for the searchbutton event.
	 * <p>
	 * The event fires when the user presses the search button on Android. If
	 * you need to override the default search button behavior on Android you
	 * can register an event listener for the 'searchbutton' event.
	 * <p>
	 * Supported platforms:
	 * <ul>
	 * <li>Android</li>
	 * </ul>
	 *
	 * @param handler
	 */
	public void addSearchButtonHandler(AppEventHandler handler);
	
	/**
	 * Removes the previously registered searchbutton handler.
	 *
	 * @param handler
	 * @see #addSearchButtonHandler(AppEventHandler)
	 */
	public void removeSearchButtonHandler(AppEventHandler handler);
	
	/**
	 * Adds an handler for the volumedownbutton event.
	 * <p>
	 * The event fires when the user presses the volume down button. If you need
	 * to override the default volume down behavior you can register an event
	 * listener for the volumedownbutton event.
	 * <p>
	 * Supported platforms:
	 * <ul>
	 * <li>Android</li>
	 * </ul>
	 *
	 * @param handler
	 */
	public void addVolumeDownButtonHandler(AppEventHandler handler);
	
	/**
	 * Removes the previously registered volumedownbutton handler.
	 *
	 * @param handler
	 * @see #addVolumeDownButtonHandler(AppEventHandler)
	 */
	public void removeVolumeDownButtonHandler(AppEventHandler handler);
	
	/**
	 * Adds an handler for the volumeupbutton event.
	 * <p>
	 * The event fires when the user presses the volume up button. If you need
	 * to override the default volume up behavior you can register an event
	 * listener for the volumeupbutton event.
	 * <p>
	 * Supported platforms:
	 * <ul>
	 * <li>Android</li>
	 * </ul>
	 *
	 * @param handler
	 */
	public void addVolumeUpButtonHandler(AppEventHandler handler);
	
	/**
	 * Removes the previously registered volumeupbutton handler.
	 *
	 * @param handler
	 * @see #addVolumeUpButtonHandler(AppEventHandler)
	 */
	public void removeVolumeUpButtonHandler(AppEventHandler handler);
}
