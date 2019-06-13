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

package com.rapidclipse.framework.server.mobilekit.barcodescanner;

import java.util.function.Consumer;

import com.rapidclipse.framework.server.mobilekit.MobileComponent;
import com.rapidclipse.framework.server.mobilekit.MobileServiceError;
import com.vaadin.flow.component.ClientCallable;
import com.vaadin.flow.component.Tag;
import com.vaadin.flow.component.dependency.HtmlImport;

import elemental.json.JsonObject;


/**
 * @author XDEV Software
 *
 */
@Tag("mobilekit-barcodescanner")
@HtmlImport("barcodescanner.html")
public class BarcodescannerComponent extends MobileComponent implements BarcodescannerService
{
	public BarcodescannerComponent()
	{
		super();
	}

	@Override
	public void scan(
		final BarcodescannerOptions options,
		final Consumer<Barcode> successCallback,
		final Consumer<MobileServiceError> errorCallback)
	{
		final String id = registerCall(successCallback, errorCallback);
		getElement().callJsFunction("scan", id, toJson(options));
	}

	@ClientCallable
	void san_success(final String id, final JsonObject barcodeObj)
	{
		final Barcode barcode = toJava(barcodeObj, BarcodeImpl.class);
		getAndRemoveCall(id).success(barcode);
	}

	@ClientCallable
	void scan_error(final String id, final String errorMessage)
	{
		final MobileServiceError error = new MobileServiceError(this, errorMessage);
		getAndRemoveCall(id).error(error);
	}

	private static class BarcodeImpl implements Barcode
	{
		private final BarcodeFormat format;
		private final String        data;

		@SuppressWarnings("unused") // Used by Gson via reflection
		public BarcodeImpl(final BarcodeFormat format, final String data)
		{
			super();
			this.format = format;
			this.data   = data;
		}

		@Override
		public BarcodeFormat getFormat()
		{
			return this.format;
		}

		@Override
		public String getData()
		{
			return this.data;
		}
	}
}
