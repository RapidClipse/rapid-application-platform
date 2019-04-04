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

/**
 * Supported formats of the {@link BarcodescannerService}.
 * <p>
 * For more information about platform support see {@link BarcodescannerService}
 *
 * @author XDEV Software
 *
 */
public enum BarcodeFormat
{
	/**
	 * Quick response matrix barcode
	 *
	 * @see <a href="https://en.wikipedia.org/wiki/QR_code">Wikipedia</a>
	 */
	QR_CODE,
	
	/**
	 * Two dimensional matrix barcode
	 *
	 * @see <a href="https://en.wikipedia.org/wiki/Data_Matrix">Wikipedia</a>
	 */
	DATA_MATRIX,
	
	/**
	 * Universal Product Code
	 *
	 * @see <a href="https://en.wikipedia.org/wiki/Universal_Product_Code">
	 *      Wikipedia</a>
	 */
	UPC_E,
	
	/**
	 * Universal Product Code
	 *
	 * @see <a href="https://en.wikipedia.org/wiki/Universal_Product_Code">
	 *      Wikipedia</a>
	 */
	UPC_A,
	
	/**
	 * International Article Number
	 *
	 * @see <a href="https://en.wikipedia.org/wiki/EAN-8">Wikipedia</a>
	 */
	EAN_8,
	
	/**
	 * International Article Number
	 *
	 * @see <a href=
	 *      "https://en.wikipedia.org/wiki/International_Article_Number_(EAN)">
	 *      Wikipedia</a>
	 */
	EAN_13,
	
	/**
	 * High-density barcode
	 *
	 * @see <a href="https://en.wikipedia.org/wiki/Code_128">Wikipedia</a>
	 */
	CODE_128,
	
	/**
	 * Variable length, discrete barcode
	 *
	 * @see <a href="https://en.wikipedia.org/wiki/Code_39">Wikipedia</a>
	 */
	CODE_39,
	
	/**
	 * Enhanced version of {@link #CODE_39}
	 *
	 * @see <a href="https://en.wikipedia.org/wiki/Code_93">Wikipedia</a>
	 */
	CODE_93,
	
	/**
	 * Linear barcode
	 *
	 * @see <a href="https://en.wikipedia.org/wiki/Codabar">Wikipedia</a>
	 */
	CODABAR,
	
	/**
	 * Interleaved 2 of 5 barcode to encode a Global Trade Item Number
	 *
	 * @see <a href="https://en.wikipedia.org/wiki/ITF-14">Wikipedia</a>
	 */
	ITF,
	
	/**
	 * Reduced Space Symbology
	 *
	 * @see <a href="https://en.wikipedia.org/wiki/GS1_DataBar">Wikipedia</a>
	 */
	RSS14,
	
	/**
	 * Portable Data File, a stacked linear barcode symbol format
	 *
	 * @see <a href="https://en.wikipedia.org/wiki/PDF417">Wikipedia</a>
	 */
	PDF417,
	
	/**
	 * Reduced Space Symbology
	 *
	 * @see <a href="https://en.wikipedia.org/wiki/GS1_DataBar">Wikipedia</a>
	 */
	RSS_EXPANDED,
	
	/**
	 * Barcode symbology developed by the MSI Data Corporation, based on the
	 * original Plessey Code
	 *
	 * @see <a href="https://en.wikipedia.org/wiki/MSI_Barcode">Wikipedia</a>
	 */
	MSI,
	
	/**
	 * 2D barcode
	 *
	 * @see <a href="https://en.wikipedia.org/wiki/Aztec_Code">Wikipedia</a>
	 */
	AZTEC,
	
	/**
	 * Unknown barcode format, used if the format of the barcode cannot be
	 * determined.
	 */
	UNKNOWN
}
