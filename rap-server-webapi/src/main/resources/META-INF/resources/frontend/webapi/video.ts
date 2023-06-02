import { LitElement, html } from 'lit';
import { customElement, property, query } from 'lit/decorators.js';

interface SourceDto {
    src: string,
    type: string
};

function compareSourceDtoArrays(a: SourceDto[] | null, b: SourceDto[] | null): boolean {
    // If both are null
    if(a == null && b == null) {
        return false;
    }
    // If only one of them is null
    if(a == null || b == null) {
        return true;
    }
    // If they don't have the same length
    if(a.length !== b.length) {
        return true;
    }
    // If one element doesn't match their counterpart
    for(let i = 0; i < a.length; i++) {
        if(a[i].src !== b[i].src || a[i].type !== b[i].type) {
            return true;
        }
    }
    // Otherwise nothing changed
    return false;
}

@customElement('rap-video')
export class RapVideo extends LitElement {
    
    @property()
    public controls: boolean = true;
    @query('#rapvideo')
    private rapvideo: HTMLVideoElement;
    private recorder: MediaRecorder | null;
    private _sources: SourceDto[] = [];

    // TODO: Find out why the video is only rendered after play() is called
    @property({ hasChanged: compareSourceDtoArrays })
    get sources(): SourceDto[] { return this._sources; }
    set sources(newSources: readonly SourceDto[]) {
        this._sources.length = 0;
        this._sources.push(...newSources);
    }

    constructor() {
        super();
    }
    
    render() {
		return html`
			<video id='rapvideo' ?controls='${this.controls}' width="100%" height="100%">
				${this.sources.map(s =>
					html`<source src='${s.src}' type='${s.type}' />` 
                )}
	        </video>
		`;
	}

    addDeviceCameraVideoSource(withVideo: boolean, withAudio: boolean, withFrontCamera: boolean): void {
        let video: any = false;

        if(withVideo) video = { facingMode: { ideal: withFrontCamera ? 'user' : 'environment' }};

        navigator.mediaDevices
            .getUserMedia({ audio: withAudio, video })
            .then(s => this.rapvideo.srcObject = s);
    }

    startRecording(): void {
        const recorder = new MediaRecorder(this.rapvideo.srcObject as MediaStream);
        
        let isFirstChunk = true;

        // When a chunk becomes available send it to the server
        recorder.ondataavailable = e => {
            const reader = new FileReader();
            reader.onloadend = () => this.$server.receiveRecordChunk(reader.result);
            reader.readAsDataURL(e.data);

            if(isFirstChunk) {
                isFirstChunk = false;
                this.$server.onRecordStart(recorder.mimeType)
            }
        }

        // Record 5 second chunks instead of one massive blob that might not be able to be transfered to the server
        recorder.start(5000);
        
        this.recorder = recorder;
    }

    stopRecording(): void {
        if(this.recorder !== null) {
            this.recorder.stop();
            this.recorder = null;
        }
    }
    
    play() {
    	this.rapvideo.play();
    }
    
    pause() {
    	this.rapvideo.pause();
    }

    takePicture() {
        if(this.rapvideo.srcObject === null) {
            return;
        }

        let canvas = document.createElement('canvas');
        canvas.width = this.rapvideo.videoWidth;
        canvas.height = this.rapvideo.videoHeight;
        canvas.getContext('2d')!.drawImage(this.rapvideo, 0, 0, this.rapvideo.videoWidth, this.rapvideo.videoHeight);
        canvas.toBlob(b => {
            const reader = new FileReader();
            reader.onloadend = () => this.$server.onPictureReceived(reader.result);
            reader.readAsDataURL(b!);
        });
    }
}
