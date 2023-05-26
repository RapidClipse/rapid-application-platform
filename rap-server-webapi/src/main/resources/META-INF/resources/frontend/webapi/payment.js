import { LitElement } from 'lit';

class RapPayment extends LitElement {
	static get is() {
		return 'rap-payment';
	}

    requestPayment(methodData, details, options) {
        let paymentRequest = new PaymentRequest(methodData, details, options);
        paymentRequest.show()
            .then(result => { this.result = result; this.$server.onRequestPaymentResultReceived(result); })
            .catch(e =>  
            	this.$server.onRequestPaymentErrorReceived({
            		name: e.name, 
            		message: e.message})
            );
    }

    completePayment(result) {
        this.result.complete(result).catch(e => 
        	this.$server.onCompleteErrorReceived({name: e.name, message: e.message}));
    }

    retryPayment(errorFields) {
        this.result.retry(errorFields)
            .then(e => this.$server.onRetryResultReceived(this.result))
            .catch(e => this.$server.onRetryErrorReceived({message: e.message, name: e.name}));
    }
}

customElements.define(RapPayment.is, RapPayment);
export default RapPayment;
