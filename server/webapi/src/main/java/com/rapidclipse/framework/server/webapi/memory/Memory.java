
package com.rapidclipse.framework.server.webapi.memory;

import com.googlecode.gentyref.TypeToken;
import com.rapidclipse.framework.server.webapi.JsonUtils;
import com.vaadin.flow.component.UI;
import com.vaadin.flow.function.SerializableConsumer;


/**
 * This class allows for querying the current devices memory as well as querying storage estimates.
 * 
 * @author XDEV Software
 * @since 10.02.00
 */
public final class Memory
{
	/**
	 * Retrieve the devices amount of memory. This is only a rough value with a max and min amount to protect users with
	 * low- and high-end devices.
	 *
	 * @param callback
	 *            The callback triggered when the devices memory amount was received.
	 */
	public static void getDeviceMemory(final SerializableConsumer<Double> callback)
	{
		UI.getCurrent()
			.getPage()
			.executeJs("return navigator.deviceMemory")
			.then(Double.class, callback);
	}
	
	/**
	 * Retrive the storage estimate. It contains the quota and the usage amount in bytes.
	 *
	 * @param callback
	 *            The callback triggered when the storage estimate was received.
	 */
	public static void getStorageEstimate(final SerializableConsumer<StorageEstimate> callback)
	{
		UI.getCurrent()
			.getPage()
			.executeJs(
				"return (async function() {const est = await navigator.storage.estimate(); return { quota: est.quota, usage: est.usage }; })()")
			.then(json -> {
				final StorageEstimate estimate = JsonUtils.GSON.fromJson(json.toJson(), new TypeToken<StorageEstimate>()
				{}.getType());
				callback.accept(estimate);
			});
	}
	
	private Memory()
	{
		throw new Error();
	}
}
