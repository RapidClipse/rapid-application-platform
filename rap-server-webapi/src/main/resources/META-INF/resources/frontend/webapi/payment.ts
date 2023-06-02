import { LitElement, html } from 'lit';
import { customElement } from 'lit/decorators.js';

interface PaymentRequestOptions {
	requestPayerName?: boolean,
	requestPayerEmail?: boolean,
	requestPayerPhone?: boolean,
	requestShipping?: boolean,
	shippingType?: string
}

@customElement('rap-payment')
export class RapPayment extends LitElement {
	
	private result: PaymentResponse;
	
	// Required so api users don't get warning spam in their logs
	render() {
		return html``;
	}

    requestPayment(methodData: PaymentMethodData[], details: PaymentDetailsInit, options?: PaymentRequestOptions) {
        let paymentRequest = new PaymentRequest(methodData, details, options);
        paymentRequest.show()
            .then(r => { this.result = r; this.$server.onRequestPaymentResultReceived(this.result); })
            .catch(e => this.$server.onRequestPaymentErrorReceived(this.cloneErrorObject(e)));
    }
	
	// Copy over values as for some reason the server would only receive empty fields
	private cloneErrorObject(e: DOMException) {
		const cloned = { name: e.name, message: e.message };
		return cloned;
	}

    completePayment(result?: PaymentComplete) {
        this.result.complete(result).catch(e => 
        	this.$server.onCompleteErrorReceived({name: e.name, message: e.message}));
    }

    retryPayment(errorFields?: PaymentValidationErrors) {
        this.result.retry(errorFields)
            .then(e => this.$server.onRetryResultReceived(this.result))
            .catch(e => this.$server.onRetryErrorReceived({message: e.message, name: e.name}));
    }
}
