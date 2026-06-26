/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.webapi.online;

import com.rapidclipse.framework.server.webapi.JavascriptTemplate;
import com.vaadin.flow.component.ClientCallable;
import com.vaadin.flow.component.HasElement;
import com.vaadin.flow.component.Tag;
import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.dependency.JsModule;
import com.vaadin.flow.function.SerializableConsumer;
import com.vaadin.flow.shared.Registration;


/**
 * With this class you can check the current online status of the device. More importantly you can register listeners
 * for when the device goes online or offline.
 *
 * @author XDEV Software
 * @since 10.02.00
 */
@JsModule("./webapi/online.ts")
@Tag("rap-online")
public class Online extends JavascriptTemplate
{
	public Online(final HasElement parent)
	{
		super(parent);
	}
	
	/**
	 * Register an online listener. This listener is triggered when the client comes back online after being
	 * offline or vice versa. To unregister this event you can either use the returned Registration or by calling
	 * {@link #unregisterAllOnlineListeners()}.
	 */
	public Registration addOnlineListener(final SerializableConsumer<OnlineState> onOnline)
	{
		final Runnable firstAddedCallback  = () -> this.getElement().callJsFunction("registerOnlineListener");
		final Runnable lastRemovedCallback = () -> this.getElement().callJsFunction("unregisterOnlineListener");
		return this.registerConsumer(OnlineState.class, onOnline, firstAddedCallback, lastRemovedCallback);
	}
	
	/**
	 * This method can be used to unregister previously registered onOnLine listeners. This will also stop the client
	 * from sending more events to the server. To register an onOnLine listener
	 * you can call the {@link #addOnlineListener(SerializableConsumer)} method.
	 */
	public void unregisterAllOnlineListeners()
	{
		final Runnable unregisterCallback = () -> this.getElement().callJsFunction("unregisterOnlineListener");
		this.unregisterAllConsumers(OnlineState.class, unregisterCallback);
	}
	
	/**
	 * Get the current OnLine state of the device (The state can not be retrieved while the client is offline).
	 *
	 * @param onStateReceived
	 *            The callback triggered when the state is received. It consumes the current onLine state.
	 */
	public static void getOnlineState(final SerializableConsumer<OnlineState> onStateReceived)
	{
		UI.getCurrent().getPage().executeJs("return navigator.onLine").then(Boolean.class,
			isOnline -> onStateReceived.accept(isOnline ? OnlineState.ONLINE : OnlineState.OFFLINE));
	}
	
	@ClientCallable
	private void onOnline()
	{
		this.notifyConsumers(OnlineState.class, OnlineState.ONLINE);
	}
	
	@ClientCallable
	private void onOffline()
	{
		this.notifyConsumers(OnlineState.class, OnlineState.OFFLINE);
	}
}
