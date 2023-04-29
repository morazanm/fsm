import { FSMInterfacePayload } from '../types/machine';
import net from 'net';

type SocketReponse<T> = {
  data: T
  error: string | null
}

export enum Instruction {
  BUILD = 'build_machine',
}

export class RacketInterface {
  private port: number
  private host: string;
  public client: net.Socket | undefined;

  constructor(port: number, host: string) {
    this.port = port;
    this.host = host;
    this.client = undefined;
  }

  extablishConnection(): boolean {
    try {
      console.log("Trying to connect...")
      this.client = net.createConnection({ port: this.port, host: this.host }, () => {
        console.log('connected to server!');
      });
      return true;
    } catch(_) {
      console.log("Was not able to connect to server")
      return false;
    }
  }

  sendToRacket<T extends object>(
    data: T,
    instruction: Instruction,
  ) {
    const jsonObj = JSON.stringify({ instr: instruction, data: data });
    if (this.client) {
      this.client.write(`${jsonObj}\r\n`);
    }
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


