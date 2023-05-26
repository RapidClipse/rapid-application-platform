import { LitElement, html } from 'lit';
import { customElement } from 'lit/decorators.js';

@customElement('rap-network')
export class RapNetwork extends LitElement {
	
	// Required so api users don't get warning spam in their logs
	render() {
		return html``;
	}
	
	// Copy over values as for some reason the server would only receive empty fields
	createNetworkInformation() {
		const n = navigator.connection;
		return {
			downlink: n.downlink,
			downlinkMax: n.downlinkMax,
			effectiveType: n.effectiveType,
			rtt: n.rtt,
			saveData: n.saveData,
			type: n.type
		};
	}

	registerOnChangeListener() {
		navigator.connection.onchange = _e => {
			this.$server.onNetworkInformationChanged(this.createNetworkInformation());
		}
	}

	unregisterOnChangeListener() {
		navigator.connection.onchange = undefined;
	}
}
