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

package com.rapidclipse.framework.server.mobilekit;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import java.util.function.Consumer;

import com.google.gson.Gson;
import com.vaadin.flow.component.polymertemplate.PolymerTemplate;
import com.vaadin.flow.templatemodel.TemplateModel;

import elemental.json.Json;
import elemental.json.JsonObject;


/**
 * @author XDEV Software
 *
 */
public abstract class MobileComponent extends PolymerTemplate<TemplateModel>
{
	private final transient Map<String, ServiceCall<?, ?>> serviceCalls = Collections
		.synchronizedMap(new HashMap<>());

	protected MobileComponent()
	{
		super();
	}

	protected JsonObject toJson(final Object pojo)
	{
		return Json.parse(new Gson().toJson(pojo));
	}

	protected <T> T toJava(final JsonObject obj, final Class<T> type)
	{
		return new Gson().fromJson(obj.toJson(), type);
	}

	protected <R, E extends MobileServiceError> String registerCall(
		final Consumer<R> successCallback,
		final Consumer<E> errorCallback)
	{
		return registerCall(ServiceCall.New(successCallback, errorCallback));
	}

	protected <R, E extends MobileServiceError> String registerCall(
		final ServiceCall<R, E> call)
	{
		final String id = UUID.randomUUID().toString();
		this.serviceCalls.put(id, call);
		return id;
	}

	@SuppressWarnings("unchecked")
	protected <R, E extends MobileServiceError> ServiceCall<R, E> getCall(final String id)
	{
		return (ServiceCall<R, E>)this.serviceCalls.get(id);
	}

	@SuppressWarnings("unchecked")
	protected <R, E extends MobileServiceError, S extends ServiceCall<R, E>> S getAndRemoveCall(
		final String id)
	{
		return (S)this.serviceCalls.remove(id);
	}

	protected void removeCall(final String id)
	{
		this.serviceCalls.remove(id);
	}

	protected static interface ServiceCall<R, E extends MobileServiceError>
	{
		public static <R, E extends MobileServiceError> ServiceCall<R, E> New(
			final Consumer<R> successCallback,
			final Consumer<E> errorCallback)
		{
			return new Implementation<R, E>(successCallback, errorCallback);
		}

		public void success(final R returnValue);

		public void error(final E error);

		public static class Implementation<R, E extends MobileServiceError>
			implements ServiceCall<R, E>
		{
			private final Consumer<R> successCallback;
			private final Consumer<E> errorCallback;

			public Implementation(
				final Consumer<R> successCallback,
				final Consumer<E> errorCallback)
			{
				this.successCallback = successCallback;
				this.errorCallback   = errorCallback;
			}

			@Override
			public void success(final R returnValue)
			{
				if(this.successCallback != null)
				{
					this.successCallback.accept(returnValue);
				}
			}

			@Override
			public void error(final E error)
			{
				if(this.errorCallback != null)
				{
					this.errorCallback.accept(error);
				}
			}
		}
	}
}
