import * as net from 'net';

const END_OF_MSG = "#<eof>"

export type SocketResponse<T> = {
  data: T;
  responseType: Instruction;
  error: string | null;
};

type SocketRequest<T> = {
  data: T;
  instr: Instruction;
};

export type Connection = {
  connected: boolean;
  status: 'done' | 'attempting';
};

export enum Instruction {
  BUILD = 'build_machine',
  CLOSE = 'shut_down',
  PREBUILT = 'prebuilt_machine',
}

export class RacketInterface {
  private port: number;
  private host: string;
  public connected: boolean;
  public client: net.Socket;
  private callbacks: ((data:string) => void)[];;

  constructor(port: number, host: string) {
    this.port = port;
    this.host = host;
    this.connected = false;
    this.client = new net.Socket();
    this.callbacks = []; 
  }

  establishConnection(): Promise<boolean> {
    return new Promise((resolve) => {
      if (this.connected) return resolve(true);
      let chunks: Buffer[] = [];
      this.client
        .connect({ port: this.port, host: this.host }, () => {
          console.log('connected to server!');
          this.connected = true;
          return resolve(true);
        })
        .on('error', () => {
          console.log('error while connecting to server');
          this.connected = false;
          return resolve(false);
        })
        .on('data', (data: Buffer) => {
            const termArg = data.subarray(data.byteLength - 6, data.byteLength)
            if (termArg.toString() === END_OF_MSG) {
              chunks.push(data.subarray(0, data.byteLength - 6));
              const tmp = Buffer.concat(chunks).toString()
              this.callbacks.forEach((cb) => cb(tmp))
              chunks = [];
            } else {
              chunks.push(data)
            }
        })
    });
  }

  subscribeListener(cb: (data: string) => void): void {
    this.callbacks.push(cb)
    console.log("subscribed", this.callbacks.length)
  }

  sendToRacket<T extends object>(data: T, instruction: Instruction) {
    const request: SocketRequest<T> = {
      instr: instruction,
      data: data,
    };
    this.send(request);
  }

  closeConnection() {
    this.client.end();
    this.connected = false;
  }

  // Sends the request object to the racket backend by wrapping it in a json object.
  // NOTE: we always need to add '\r\n' at the end of a request so racket knows that
  // this is the end of the data being sent
  private send<T extends object>(request: SocketRequest<T>): boolean {
    if (this.connected) {
      this.client.write(`${JSON.stringify(request)}\r\n`);
      return true;
    }
    return false;
  }
}
