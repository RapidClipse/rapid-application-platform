import { LitElement } from 'lit';

class RapVisibility extends LitElement {
	static get is() {
		return 'rap-visibility';
    }

    registerVisibilityListener() {
        document.onvisibilitychange = () => this.$server.onVisibilityChanged();
    }

    unregisterVisibilityListener() {
        document.onvisibilitychange = undefined;
    }
}

customElements.define(RapVisibility.is, RapVisibility);
export default RapVisibility;
