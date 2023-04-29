import { FSMInterfacePayload } from '../types/machine';
let net = require('net');

const HOST = '127.0.0.1';
const PORT = 4000;

type SocketReponse<T> = {
  data: T
  error: string | null
}

const client = net.createConnection({ port: PORT, host: HOST }, () => {
  console.log('connected to server!');
});

client.on('data', (data: any) => {
  console.log("recieved:")
  console.log(JSON.parse(data.toString()));
});

client.on('end', () => {
  console.log('disconnected from server (on end)');
});

client.on('close', () => {
  console.log('disconnected from server (on close)');
});



enum Instruction {
  BUILD = 'build_machine',
}

export function sendToRacket<T extends object>(
  data: T,
  instruction: Instruction,
) {
  const jsonObj = JSON.stringify({ instr: instruction, data: data });
  client.write(`${jsonObj}\r\n`);
}

sendToRacket(
  {
    states: [
      { name: 'S', type: 'start' },
      { name: 'A', type: 'normal' },
      { name: 'F', type: 'final' },
    ],
    alphabet: ['a', 'b'],
    type: 'dfa',
    rules: [
      {start: "S", input: "a", end: "F"},
      {start: "F", input: "a", end: "F"},
      {start: "S", input: "b", end: "A"},
      {start: "A", input: "a", end: "F"},
      {start: "A", input: "b", end: "A"},
    ],
    input: ["a", "a", "a"]
  } as FSMInterfacePayload,
  Instruction.BUILD,
);


