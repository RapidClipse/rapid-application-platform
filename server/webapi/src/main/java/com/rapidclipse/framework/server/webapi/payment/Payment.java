/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
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
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 */

package com.rapidclipse.framework.server.webapi.payment;

import java.lang.reflect.Type;
import java.util.List;
import java.util.function.Consumer;

import com.google.gson.reflect.TypeToken;
import com.rapidclipse.framework.server.webapi.JavascriptError;
import com.rapidclipse.framework.server.webapi.JavascriptTemplate;
import com.rapidclipse.framework.server.webapi.JsonUtils;
import com.vaadin.flow.component.ClientCallable;
import com.vaadin.flow.component.HasElement;
import com.vaadin.flow.component.Tag;
import com.vaadin.flow.component.dependency.HtmlImport;
import com.vaadin.flow.templatemodel.TemplateModel;

import elemental.json.JsonObject;


/**
 * With this class you can ask the browser to display an payment interface. It lets the user select his preferred way of
 * paying and will also cache his information so he does not have to retype his information.
 * 
 * @author XDEV Software
 * @since 10.02.00
 */
@HtmlImport("frontend://webapi/payment.html")
@Tag("rap-payment")
public class Payment extends JavascriptTemplate<Payment.PaymentTemplateModel>
{
	private Consumer<PaymentResult>   onRequestPaymentResultReceived;
	private Consumer<JavascriptError> onRequestPaymentErrorReceived;
	private Consumer<PaymentResult>   onRetryResultReceived;
	private Consumer<JavascriptError> onRetryErrorReceived;
	private Consumer<JavascriptError> onCompleteErrorReceived;

	public Payment(final HasElement parent)
	{
		parent.getElement().appendVirtualChild(this.getElement());
	}

	/**
	 * Request the browser to display the payment interface. The user can then input his information and preferred
	 * payment method. When the user is done and the dialog was successful
	 * the <b>onResultReceived</b> callback is triggered. If the user aborts the dialog or something goes wrong the
	 * <b>onErrorReceived</b> callback will be triggered.
	 *
	 * @param methodData
	 *            A List of different payment methods that are accepted. Currently only basic-card is available.
	 * @param details
	 *            Details such as total and a list of items. This is to show the user the list of items that he will
	 *            receive, once the transaction is over.
	 * @param options
	 *            Various options such as requesting the user to input his email or phone number.
	 * @param onResultReceived
	 *            The callback triggered when the dialog was successful and the user clicked on pay. It will consume a
	 *            PaymentResult that contains information needed for the payment.
	 * @param onErrorReceived
	 *            The callback triggered when the dialog was aborted by the user or something else went wrong. It will
	 *            consume the JavaScript error that was thrown on the client side.
	 */
	public void requestPayment(
		final List<PaymentRequestMethodData> methodData,
		final PaymentRequestDetails details,
		final PaymentRequestOptions options,
		final Consumer<PaymentResult> onResultReceived,
		final Consumer<JavascriptError> onErrorReceived)
	{
		this.onRequestPaymentResultReceived = onResultReceived;
		this.onRequestPaymentErrorReceived  = onErrorReceived;
		this.getElement()
			.callJsFunction("requestPayment",
				JsonUtils.encodeObject(methodData),
				JsonUtils.encodeObject(details),
				JsonUtils.encodeObject(options));
	}

	/**
	 * This method can be used if a payment request failed and the error is recoverable. It will redisplay the payment
	 * interface. With the <b>errorFields</b> parameter the invalid fields can be highlighted.
	 *
	 * @param errorFields
	 *            The fields that made the error occur.
	 * @param onResultReceived
	 *            The callback triggered when the dialog was successful and the user clicked on pay. It will consume a
	 *            PaymentResult that contains information needed for the payment.
	 * @param onErrorOccurred
	 *            The callback triggered if the dialog fails again. It conumes the JavaScript error that is thrown on
	 *            the client side.
	 */
	public void retry(
		final PaymentValidationError errorFields,
		final Consumer<PaymentResult> onResultReceived,
		final Consumer<JavascriptError> onErrorOccurred)
	{
		this.onRetryResultReceived = onResultReceived;
		this.onRetryErrorReceived  = onErrorOccurred;

		this.getElement()
			.callJsFunction("retryPayment",
				JsonUtils.encodeObject(errorFields));
	}

	/**
	 * This method can be used to show the user that the transaction is complete.
	 *
	 * @param result
	 *            The result of the transaction. The {@link Result} enum is used for if the transaction has succeeded
	 *            or failed.
	 * @param onErrorOccurred
	 *            The callback triggered when something goes wrong and the payment can not be completed. It will consume
	 *            the JavaScript error thrown on the client side.
	 */
	public void complete(final Result result, final Consumer<JavascriptError> onErrorOccurred)
	{
		this.onCompleteErrorReceived = onErrorOccurred;
		this.getElement()
			.callJsFunction("completePayment", JsonUtils.encodeObject(result));
	}

	@ClientCallable
	private void onRequestPaymentResultReceived(final JsonObject result)
	{
		if(this.onRequestPaymentResultReceived != null)
		{
			final Type t = new TypeToken<PaymentResult>()
			{}.getType();
			this.onRequestPaymentResultReceived.accept(JsonUtils.GSON.fromJson(result.toJson(), t));
		}
	}

	@ClientCallable
	private void onRequestPaymentErrorReceived(final JsonObject error)
	{
		if(this.onRequestPaymentErrorReceived != null)
		{
			final Type t = new TypeToken<JavascriptError>()
			{}.getType();
			this.onRequestPaymentErrorReceived.accept(JsonUtils.GSON.fromJson(error.toJson(), t));
		}
	}

	@ClientCallable
	private void onRetryResultReceived(final JsonObject result)
	{
		if(this.onRetryResultReceived != null)
		{
			final Type t = new TypeToken<PaymentResult>()
			{}.getType();
			this.onRetryResultReceived.accept(JsonUtils.GSON.fromJson(result.toJson(), t));
		}
	}

	@ClientCallable
	private void onRetryErrorReceived(final JsonObject error)
	{
		if(this.onRetryErrorReceived != null)
		{
			final Type t = new TypeToken<JavascriptError>()
			{}.getType();
			this.onRetryErrorReceived.accept(JsonUtils.GSON.fromJson(error.toJson(), t));
		}
	}

	@ClientCallable
	private void onCompleteErrorReceived(final JsonObject error)
	{
		if(this.onCompleteErrorReceived != null)
		{
			final Type t = new TypeToken<JavascriptError>()
			{}.getType();
			this.onCompleteErrorReceived.accept(JsonUtils.GSON.fromJson(error.toJson(), t));
		}
	}

	/**
	 * The different results the payment completion can have.
	 */
	public static enum Result
	{
		/**
		 * This will signal the user that the transaction has completed successfully.
		 */
		success,

		/**
		 * This will signal the user that the transaction has failed.
		 */
		fail,

		/**
		 * This will signal the user that the transaction status is unkown.
		 */
		unkown
	}

	public static interface PaymentTemplateModel extends TemplateModel
	{
	}
}
