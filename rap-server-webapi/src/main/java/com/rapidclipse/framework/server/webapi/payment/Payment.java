/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.webapi.payment;

import java.util.List;
import java.util.function.Consumer;

import com.rapidclipse.framework.server.webapi.JavascriptError;
import com.rapidclipse.framework.server.webapi.JavascriptTemplate;
import com.rapidclipse.framework.server.webapi.JsonUtils;
import com.vaadin.flow.component.ClientCallable;
import com.vaadin.flow.component.HasElement;
import com.vaadin.flow.component.Tag;
import com.vaadin.flow.component.dependency.JsModule;

import elemental.json.JsonObject;

/**
 * With this class you can ask the browser to display an payment interface. It
 * lets the user select his preferred way of paying and will also cache his
 * information so he does not have to retype his information.
 * 
 * @author XDEV Software
 * @since 10.02.00
 */
@JsModule("./webapi/payment.ts")
@Tag("rap-payment")
public class Payment extends JavascriptTemplate
{
	private Consumer<PaymentResult> onRequestPaymentResultReceived;
	private Consumer<JavascriptError> onRequestPaymentErrorReceived;
	private Consumer<PaymentResult> onRetryResultReceived;
	private Consumer<JavascriptError> onRetryErrorReceived;
	private Consumer<JavascriptError> onCompleteErrorReceived;

	public Payment(final HasElement parent)
	{
		parent.getElement().appendVirtualChild(this.getElement());
	}

	/**
	 * Request the browser to display the payment interface. The user can then input
	 * his information and preferred payment method. When the user is done and the
	 * dialog was successful the <b>onResultReceived</b> callback is triggered. If
	 * the user aborts the dialog or something goes wrong the <b>onErrorReceived</b>
	 * callback will be triggered.
	 *
	 * @param methodData       A List of different payment methods that are
	 *                         accepted. Currently only basic-card is available.
	 * @param details          Details such as total and a list of items. This is to
	 *                         show the user the list of items that he will receive,
	 *                         once the transaction is over.
	 * @param options          Various options such as requesting the user to input
	 *                         his email or phone number.
	 * @param onResultReceived The callback triggered when the dialog was successful
	 *                         and the user clicked on pay. It will consume a
	 *                         PaymentResult that contains information needed for
	 *                         the payment.
	 * @param onErrorReceived  The callback triggered when the dialog was aborted by
	 *                         the user or something else went wrong. It will
	 *                         consume the JavaScript error that was thrown on the
	 *                         client side.
	 */
	public void requestPayment(
		final List<PaymentRequestMethodData> methodData,
		final PaymentRequestDetails details,
		final PaymentRequestOptions options,
		final Consumer<PaymentResult> onResultReceived,
		final Consumer<JavascriptError> onErrorReceived
	)
	{
		this.onRequestPaymentResultReceived = onResultReceived;
		this.onRequestPaymentErrorReceived = onErrorReceived;
		this.getElement()
			.callJsFunction(
				"requestPayment",
				JsonUtils.encodeObject(methodData),
				JsonUtils.encodeObject(details),
				JsonUtils.encodeObject(options)
			);
	}

	/**
	 * This method can be used if a payment request failed and the error is
	 * recoverable. It will redisplay the payment interface. With the
	 * <b>errorFields</b> parameter the invalid fields can be highlighted.
	 *
	 * @param errorFields      The fields that made the error occur.
	 * @param onResultReceived The callback triggered when the dialog was successful
	 *                         and the user clicked on pay. It will consume a
	 *                         PaymentResult that contains information needed for
	 *                         the payment.
	 * @param onErrorOccurred  The callback triggered if the dialog fails again. It
	 *                         conumes the JavaScript error that is thrown on the
	 *                         client side.
	 */
	public void retry(
		final PaymentValidationError errorFields,
		final Consumer<PaymentResult> onResultReceived,
		final Consumer<JavascriptError> onErrorOccurred
	)
	{
		this.onRetryResultReceived = onResultReceived;
		this.onRetryErrorReceived = onErrorOccurred;

		this.getElement().callJsFunction("retryPayment", JsonUtils.encodeObject(errorFields));
	}

	/**
	 * This method can be used to show the user that the transaction is complete.
	 *
	 * @param result          The result of the transaction. The {@link Result} enum
	 *                        is used for if the transaction has succeeded or
	 *                        failed.
	 * @param onErrorOccurred The callback triggered when something goes wrong and
	 *                        the payment can not be completed. It will consume the
	 *                        JavaScript error thrown on the client side.
	 */
	public void complete(final Result result, final Consumer<JavascriptError> onErrorOccurred)
	{
		this.onCompleteErrorReceived = onErrorOccurred;
		this.getElement().callJsFunction("completePayment", JsonUtils.encodeObject(result));
	}

	@ClientCallable
	private void onRequestPaymentResultReceived(final JsonObject result)
	{
		if (this.onRequestPaymentResultReceived != null)
		{
			final var retval = new PaymentResult(
				result.getObject("details"),
				result.getString("methodName"),
				result.getString("payerEmail"),
				result.getString("payerName"),
				result.getString("payerPhone"),
				result.getString("requestId"),
				JsonUtils.GSON.fromJson(result.getObject("shippingAddress").toString(), PaymentAddress.class),
				result.getString("shippingOption")
			);
			this.onRequestPaymentResultReceived.accept(retval);
		}
	}

	@ClientCallable
	private void onRequestPaymentErrorReceived(final JsonObject error)
	{
		if (this.onRequestPaymentErrorReceived != null)
		{
			this.onRequestPaymentErrorReceived.accept(JsonUtils.GSON.fromJson(error.toJson(), JavascriptError.class));
		}
	}

	@ClientCallable
	private void onRetryResultReceived(final JsonObject result)
	{
		if (this.onRetryResultReceived != null)
		{
			this.onRetryResultReceived.accept(JsonUtils.GSON.fromJson(result.toJson(), PaymentResult.class));
		}
	}

	@ClientCallable
	private void onRetryErrorReceived(final JsonObject error)
	{
		if (this.onRetryErrorReceived != null)
		{
			this.onRetryErrorReceived.accept(JsonUtils.GSON.fromJson(error.toJson(), JavascriptError.class));
		}
	}

	@ClientCallable
	private void onCompleteErrorReceived(final JsonObject error)
	{
		if (this.onCompleteErrorReceived != null)
		{
			this.onCompleteErrorReceived.accept(JsonUtils.GSON.fromJson(error.toJson(), JavascriptError.class));
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
}
