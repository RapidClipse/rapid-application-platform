import { LitElement, html } from 'lit';
import { customElement } from 'lit/decorators.js';

interface TouchDto {
	identifier: number,
	screenX: number,
	screenY: number,
	clientX: number,
	clientY: number,
	pageX: number,
	pageY: number,
	radiusX: number,
	radiusY: number,
	rotationAngle: number,
	force: number
}

interface TouchEventDto {
	type: string,
	altKey: boolean,
	changedTouches: TouchDto[],
	ctrlKey: boolean,
	metaKey: boolean,
	shiftKey: boolean,
	targetTouches: TouchDto[],
	touches: TouchDto[]
}

@customElement('rap-touch')
export class RapTouch extends LitElement {
	
	// Required so api users don't get warning spam in their logs
	render() {
		return html``;
	}

	// Copy over values as for some reason the server would only receive empty fields
	private convertTouchList(l: TouchList): TouchDto[] {
		let ret: TouchDto[] = new Array(l.length);
		for(let i = 0; i < l.length; i++) {
			const t = l[i];
			ret[i] = {
				identifier: t.identifier,
				screenX: t.screenX,
				screenY: t.screenY,
				clientX: t.clientX,
				clientY: t.clientY,
				pageX: t.pageX,
				pageY: t.pageY,
				radiusX: t.radiusX,
				radiusY: t.radiusY,
				rotationAngle: t.rotationAngle,
				force: t.force
			};
		}
		return ret;
	}

	// Copy over values as for some reason the server would only receive empty fields
	private cloneTouchEvent(e: TouchEvent) {
		let retval = {
			altKey: e.altKey,
			changedTouches: this.convertTouchList(e.changedTouches),
			ctrlKey: e.ctrlKey,
			metaKey: e.metaKey,
			shiftKey: e.shiftKey,
			targetTouches: this.convertTouchList(e.targetTouches),
			touches: this.convertTouchList(e.touches)
		};
		return retval;
	}
	
	registerTouchListener(target: HTMLElement) {
		target.ontouchstart = e => this.$server.onTouchStart(this.cloneTouchEvent(e));
		target.ontouchend = e => this.$server.onTouchEnd(this.cloneTouchEvent(e));
		target.ontouchmove = e => this.$server.onTouchMove(this.cloneTouchEvent(e));
		target.ontouchcancel = e => this.$server.onTouchCancel(this.cloneTouchEvent(e));
	}
	
	unregisterTouchListener(target: HTMLElement) {
		target.ontouchstart = null;
		target.ontouchend = null;
		target.ontouchmove = null;
		target.ontouchcancel = null;
	}
}
