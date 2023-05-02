import * as net from 'net';

export type SocketResponse<T> = {
  data: T;
  responseType: Instruction;
  error: string | null;
};

type SocketRequest<T> = {
  data: T;
  instr: Instruction;
};

export enum Instruction {
  BUILD = 'build_machine',
  CLOSE = 'shut_down',
}

export class RacketInterface {
  private port: number;
  private host: string;
  public client: net.Socket | undefined;

  constructor(port: number, host: string) {
    this.port = port;
    this.host = host;
    this.client = undefined;
  }

  extablishConnection(): boolean {
    try {
      console.log('Trying to connect...');
      this.client = net.createConnection(
        { port: this.port, host: this.host },
        () => {
          console.log('connected to server!');
        },
      );
      return true;
    } catch (e) {
      console.log('Was not able to connect to server');
      return false;
    }
  }

  sendToRacket<T extends object>(data: T, instruction: Instruction) {
    const request: SocketRequest<T> = {
      instr: instruction,
      data: data,
    };
    this.send(request);
  }

  closeConnection() {
    const request: SocketRequest<{}> = { data: {}, instr: Instruction.CLOSE };
    this.send(request);
  }

  // Sends the request object to the racket backend by wrapping it in a json object.
  // NOTE: we always need to add '\r\n' at the end of a request so racket knows that
  // this is the end of the data being sent
  private send<T extends object>(request: SocketRequest<T>): boolean {
    //TODO: how to handle a broken connection
    if (this.client) {
      this.client.write(`${JSON.stringify(request)}\r\n`);
      return true;
    }
    return false;
  }
}

// const client = net.createConnection({ port: PORT, host: HOST }, () => {
//   console.log('connected to server!');
// });

// client.on('data', (data: any) => {
//   console.log("recieved:")
//   console.log(JSON.parse(data.toString()));
// });

// client.on('end', () => {
//   console.log('disconnected from server (on end)');
// });

// client.on('close', () => {
//   console.log('disconnected from server (on close)');
// });

// sendToRacket(
//   {
//     states: [
//       { name: 'S', type: 'start' },
//       { name: 'A', type: 'normal' },
//       { name: 'F', type: 'final' },
//     ],
//     alphabet: ['a', 'b'],
//     type: 'dfa',
//     rules: [
//       {start: "S", input: "a", end: "F"},
//       {start: "F", input: "a", end: "F"},
//       {start: "S", input: "b", end: "A"},
//       {start: "A", input: "a", end: "F"},
//       {start: "A", input: "b", end: "A"},
//     ],
//     input: ["a", "a", "a"]
//   } as FSMInterfacePayload,
//   Instruction.BUILD,
// );
