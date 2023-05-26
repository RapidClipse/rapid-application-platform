import { LitElement } from 'lit';
import { customElement } from 'lit/decorators.js';

@customElement('rap-network')
export class RapNetwork extends LitElement {
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
