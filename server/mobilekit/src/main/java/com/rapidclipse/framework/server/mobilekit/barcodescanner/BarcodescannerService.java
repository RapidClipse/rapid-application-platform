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

import com.rapidclipse.framework.server.mobilekit.MobileService;
import com.rapidclipse.framework.server.mobilekit.MobileServiceError;
import com.rapidclipse.framework.server.mobilekit.MobileServiceScriptConfiguration;
import com.rapidclipse.framework.server.mobilekit.Plugin;
import com.rapidclipse.framework.server.mobilekit.ServiceComponent;


/**
 * Service for scanning barcodes of various types.
 * <p>
 * The following barcode types are currently supported:
 * <p>
 * Android
 * <ul>
 * <li>{@link BarcodeFormat#QR_CODE QR_CODE}</li>
 * <li>{@link BarcodeFormat#DATA_MATRIX DATA_MATRIX}</li>
 * <li>{@link BarcodeFormat#UPC_E UPC_E}</li>
 * <li>{@link BarcodeFormat#UPC_A UPC_A}</li>
 * <li>{@link BarcodeFormat#EAN_8 EAN_8}</li>
 * <li>{@link BarcodeFormat#EAN_13 EAN_13}</li>
 * <li>{@link BarcodeFormat#CODE_128 CODE_128}</li>
 * <li>{@link BarcodeFormat#CODE_39 CODE_39}</li>
 * <li>{@link BarcodeFormat#CODE_93 CODE_93}</li>
 * <li>{@link BarcodeFormat#CODABAR CODABAR}</li>
 * <li>{@link BarcodeFormat#ITF ITF}</li>
 * <li>{@link BarcodeFormat#RSS14 RSS14}</li>
 * <li>{@link BarcodeFormat#PDF417 PDF417}</li>
 * <li>{@link BarcodeFormat#RSS_EXPANDED RSS_EXPANDED}</li>
 * </ul>
 * <p>
 * iOS
 * <ul>
 * <li>{@link BarcodeFormat#QR_CODE QR_CODE}</li>
 * <li>{@link BarcodeFormat#DATA_MATRIX DATA_MATRIX}</li>
 * <li>{@link BarcodeFormat#UPC_E UPC_E}</li>
 * <li>{@link BarcodeFormat#UPC_A UPC_A}</li>
 * <li>{@link BarcodeFormat#EAN_8 EAN_8}</li>
 * <li>{@link BarcodeFormat#EAN_13 EAN_13}</li>
 * <li>{@link BarcodeFormat#CODE_128 CODE_128}</li>
 * <li>{@link BarcodeFormat#CODE_39 CODE_39}</li>
 * <li>{@link BarcodeFormat#ITF ITF}</li>
 * </ul>
 * <p>
 * Windows
 * <ul>
 * <li>{@link BarcodeFormat#UPC_A UPC_A}</li>
 * <li>{@link BarcodeFormat#UPC_E UPC_E}</li>
 * <li>{@link BarcodeFormat#EAN_8 EAN_8}</li>
 * <li>{@link BarcodeFormat#EAN_13 EAN_13}</li>
 * <li>{@link BarcodeFormat#CODE_39 CODE_39}</li>
 * <li>{@link BarcodeFormat#CODE_93 CODE_93}</li>
 * <li>{@link BarcodeFormat#CODE_128 CODE_128}</li>
 * <li>{@link BarcodeFormat#ITF ITF}</li>
 * <li>{@link BarcodeFormat#CODABAR CODABAR}</li>
 * <li>{@link BarcodeFormat#MSI MSI}</li>
 * <li>{@link BarcodeFormat#RSS14 RSS14}</li>
 * <li>{@link BarcodeFormat#QR_CODE QR_CODE}</li>
 * <li>{@link BarcodeFormat#DATA_MATRIX DATA_MATRIX}</li>
 * <li>{@link BarcodeFormat#AZTEC AZTEC}</li>
 * <li>{@link BarcodeFormat#PDF417 PDF417}</li>
 * </ul>
 *
 * @author XDEV Software
 *
 */
@ServiceComponent(BarcodescannerComponent.class)
@MobileServiceScriptConfiguration(plugins = @Plugin(name = "phonegap-plugin-barcodescanner", spec = "8.0.1"))
public interface BarcodescannerService extends MobileService
{
	public static BarcodescannerService getCurrent()
	{
		return MobileService.getCurrent(BarcodescannerService.class);
	}
	
	/**
	 * Opens the barcode scanner and passes the result to the callback if the
	 * scan was completed successfully.
	 *
	 * @param options
	 * @param successCallback
	 *
	 * @see BarcodescannerService
	 * @see BarcodeFormat
	 * @see Barcode
	 */
	default public void scan(
		final BarcodescannerOptions options,
		final Consumer<Barcode> successCallback)
	{
		scan(options, successCallback, null);
	}
	
	/**
	 * Opens the barcode scanner and passes the result to the callback if the
	 * scan was completed successfully.
	 *
	 * @param options
	 * @param successCallback
	 * @param errorCallback
	 *
	 * @see BarcodescannerService
	 * @see BarcodeFormat
	 * @see Barcode
	 */
	public void scan(
		BarcodescannerOptions options,
		Consumer<Barcode> successCallback,
		Consumer<MobileServiceError> errorCallback);
}
