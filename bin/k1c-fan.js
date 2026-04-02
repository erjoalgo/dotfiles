#!/usr/bin/env node

const WS_URL = 'ws://k1c.arpa:9999';

class Client {
    constructor(wsUrl, maxTemp, maxLastTempTimeSecs) {
        this.wsUrl = wsUrl;
        this.socket = null;

        this.lastTemp = null;
        this.maxTemp = maxTemp || 50;
        this.maxLastTempTimeSecs = maxLastTempTimeSecs || 60;
    }

    readTemp (packet) {
        const temp = packet.nozzleTemp;
        if (temp) {
            return parseFloat(temp);
        }
    }

    _connect (onDisconnect) {
        return new Promise((function(onDisconnect, resolve, reject) {
            this.socket = new WebSocket(this.wsUrl);

            this.socket.addEventListener('open', () => {
                console.log('Connected to K1C server');
                resolve();
            });

            this.socket.addEventListener('message', this.onMessage.bind(this));

            this.socket.addEventListener('close', (event) => {
                console.log('Connection closed:', event.reason);
                reject(event.reason);
                onDisconnect(event.reason);
            });

            this.socket.addEventListener('error', (error) => {
                console.error('WebSocket error:', error);
                reject(event.reason);
                onDisconnect(event.reason);
            });
        }).bind(this, onDisconnect));
    }

    start () {
        return new Promise((async function(resolve, reject) {

            console.log(`connecting...`);
            await this._connect(reject);

            const delayMs = 1000 * this.maxLastTempTimeSecs;
            console.log(`starting switch loop, running every ${delayMs/1000} secs...`);

            setInterval((async (reject) =>  {
                try {
                    await this.switchFan();
                } catch(err) {
                    console.error(`error in switch loop: ${err}`);
                    reject(err);
                }
            }).bind(this, reject),
                        delayMs);

        }).bind(this));
    }

    onMessage (event) {
        console.debug('Received:', event.data);
        let info;
        try {
            info = JSON.parse(event.data);
        } catch(err) {
            console.error(`couldn't parse server message: ${err}`);
            return;
        }
        const temp = this.readTemp(info);
        if (!temp) {
            console.debug(`failed to read  temp from message: ${event.data}`);
            return;
        }
        console.debug(`current temp is ${temp}`);
        this.setTemp(temp);
    }

    setTemp (temp) {
        this.lastTemp = {
            temp: temp,
            time: Client.nowSecs()
        };
    }

    static nowSecs () {
        const secs = Math.floor((Date.now())) / 1000;
        return secs;
    }

    async turnBackFanOn () {
        await this.sendMessage('{"method":"set","params":{"fanCase":1}}');
    }

    async turnBackFanOff () {
        await this.sendMessage('{"method":"set","params":{"fanCase":0}}');
    }

    async sendMessage (packet) {
        console.log(`sending message: ${packet}`);
        await this.socket.send(packet + `\r\n`);
    }

    async switchFan () {
        if (!this.lastTemp ||
            this.lastTemp.temp > this.maxTemp) {
            await this.turnBackFanOn();
            return;
        }
        const elapsed = (Client.nowSecs() - this.lastTemp.stamp);
        if (elapsed > this.maxLastTempTimeSecs) {
            console.warn(`too much time has elapsed since the last known nozzle temp`);
            await this.turnBackFanOn();
            return;
        }
        await this.turnBackFanOff();
    }
}

async function main () {
    const ws_url = process.env[`WS_URL`] || WS_URL;
    const ws = new Client(ws_url);
    if (process.env[`VERBOSE`] == `false`) {
        console.log(`ignoring console.debug logs...`);
        console.debug = () => {};
    }
    await ws.start();
}

main();
