import * as net from 'net';

const END_OF_MSG = '#<eof>';

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
  REDRAW = 'redraw',
}

type SocketEvent = 'data' | 'error' | 'end';

export class RacketInterface {
  private port: number;
  private host: string;
  private subscribers: Map<SocketEvent, ((data: string) => void)[]>;
  public connected: boolean;
  public client: net.Socket;

  constructor(port: number, host: string) {
    this.port = port;
    this.host = host;
    this.connected = false;
    this.client = new net.Socket();
    this.subscribers = new Map([
      ['data', []],
      ['error', []],
      ['end', []],
    ]);
  }

  establishConnection(): Promise<boolean> {
    return new Promise((resolve) => {
      if (this.connected) return resolve(true);
      let chunks: Buffer[] = [];
      this.client
        .connect({ port: this.port, host: this.host }, () => {
          this.connected = true;
          return resolve(true);
        })
        .on('error', () => {
          this.connected = false;
          return resolve(false);
        })
        .on('data', (data: Buffer) => {
          //NOTE: Sockets do not have a standard around when the end of a stream is reached. For
          //small messages it does not matter because it will be sent in a single buffer. However,
          //for larger messages it may be sent over multiple buffers. Because of this we created a
          //simple protocol call `FSM-Protocol` that sends a EOF object to mark the end of the
          //message. One we read EOF we know that that there is no more data to be received.
          const termArg = data.subarray(data.byteLength - 6, data.byteLength);
          if (termArg.toString() === END_OF_MSG) {
            chunks.push(data.subarray(0, data.byteLength - 6));
            const tmp = Buffer.concat(chunks).toString();
            this.subscribers.get('data').forEach((fn) => fn(tmp));
            chunks = [];
          } else {
            chunks.push(data);
          }
        })
        .on('end', () => {
          this.subscribers.get('end').forEach((fn) => fn('end'));
          this.closeConnection();
        });
    });
  }

  subscribeListener(event: SocketEvent, cb: (data: string) => void): void {
    const tmp = this.subscribers.get(event);
    tmp.push(cb);
    this.subscribers.set(event, tmp);
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
    if (this.connected && this.client.readyState === 'open') {
      console.log(this.client.readyState);
      this.client.write(`${JSON.stringify(request)}\r\n`);
      return true;
    }
    return false;
  }
}
