/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.webapi.network;

import java.util.function.Consumer;

import com.rapidclipse.framework.server.webapi.JavascriptTemplate;
import com.rapidclipse.framework.server.webapi.JsonUtils;
import com.vaadin.flow.component.ClientCallable;
import com.vaadin.flow.component.HasElement;
import com.vaadin.flow.component.Tag;
import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.dependency.JsModule;
import com.vaadin.flow.function.SerializableConsumer;
import com.vaadin.flow.shared.Registration;

import elemental.json.JsonObject;
import elemental.json.JsonValue;


/**
 * This class lets you query the current devices network information. With this information you can for example determin
 * how much
 * data you want to sent to the client if the device is running on save data. You can also register listeners that are
 * notified when this information changes.
 * 
 * @author XDEV Software
 * @since 10.02.00
 */
@JsModule("./webapi/network.ts")
@Tag("rap-network")
public class Network extends JavascriptTemplate
{
	public Network(final HasElement parent)
	{
		super(parent);
	}
	
	/**
	 * Add a listener that will be notified when the network information of the current device has
	 * changed. The listener can be unregistered with the returned Registration.
	 *
	 * @param listener
	 *            The listener that is triggered everytime the network information changes.
	 */
	public Registration addNetworkInformationListener(final SerializableConsumer<NetworkInformation> listener)
	{
		final Runnable firstAddedCallback  = () -> this.getElement().callJsFunction("registerOnChangeListener");
		final Runnable lastRemovedCallback = () -> this.getElement().callJsFunction("unregisterOnChangeListener");
		return this.registerConsumer(NetworkInformation.class, listener, firstAddedCallback, lastRemovedCallback);
	}
	
	/**
	 * Gather network information from the device and send it to the server via the <b>onInformationReceived</b>
	 * callback.
	 *
	 * @param callback
	 *            The callback that consumes the received network information.
	 */
	public static void getNetworkInformation(final Consumer<NetworkInformation> callback)
	{
		UI.getCurrent()
			.getPage()
			.executeJs(
				"const n = navigator.connection;" +
					"	return {" +
					"		downlink: n.downlink," +
					"		downlinkMax: n.downlinkMax," +
					"		effectiveType: n.effectiveType," +
					"		rtt: n.rtt," +
					"		saveData: n.saveData," +
					"		type: n.type" +
					"		};")
			.then(obj -> callback.accept(parseNetworkInformation(obj)));
	}
	
	/**
	 * Clears the listeners registered with {@link #addNetworkInformationListener(SerializableConsumer)} and stops the
	 * client from sending more network information change events.
	 */
	public void clearWatch()
	{
		this.getElement().callJsFunction("unregisterOnChangeListener");
		this.clearConsumers();
	}
	
	@ClientCallable
	private void onNetworkInformationChanged(final JsonObject obj)
	{
		this.notifyConsumers(NetworkInformation.class, parseNetworkInformation(obj));
	}
	
	/**
	 * Static utility method for parsing incoming network information objects serialized in json.
	 *
	 * @param json
	 *            The json object to parse.
	 */
	private static NetworkInformation parseNetworkInformation(final JsonValue json)
	{
		return JsonUtils.GSON.fromJson(json.toJson(), NetworkInformation.class);
	}
}
