import { LitElement } from 'lit';

class RapOnline extends LitElement {
	static get is() {
		return 'rap-online';
	}

	registerOnlineListener() {
		window.ononline = _e => this.$server.onOnline();
		window.onoffline = _e => this.$server.onOffline();
	}

	unregisterOnlineLineListener() {
		window.ononline = undefined;
		window.onoffline = undefined;
	}
}

customElements.define(RapOnline.is, RapOnline);
export default RapOnline;
