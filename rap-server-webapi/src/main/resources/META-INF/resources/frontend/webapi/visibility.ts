import { LitElement, html } from 'lit';
import { customElement } from 'lit/decorators.js';

@customElement('rap-visibility')
export class RapVisibility extends LitElement {

	// Required so api users don't get warning spam in their logs
    render() {
		return html``;
	}

    registerVisibilityListener() {
        document.onvisibilitychange = () => this.$server.onVisibilityChanged();
    }

    unregisterVisibilityListener() {
        document.onvisibilitychange = undefined;
    }
}
