import { LitElement, html } from 'lit';
import { customElement } from 'lit/decorators.js';

@customElement('rap-online')
export class RapOnline extends LitElement {
    
	// Required so api users don't get warning spam in their logs
	render() {
		return html``;
	}

	registerOnlineListener() {
		window.ononline = _e => this.$server.onOnline();
		window.onoffline = _e => this.$server.onOffline();
	}

	unregisterOnlineLineListener() {
		window.ononline = null;
		window.onoffline = null;
	}
}
