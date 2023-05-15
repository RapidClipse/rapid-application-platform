/*
 * Copyright (C) 2013-2022 by XDEV Software, All Rights Reserved.
 *
 * This file is part of the RapidClipse Application Platform (RAP).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software - initial API and implementation
 */
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
