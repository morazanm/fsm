let net = require('net');

const HOST = '127.0.0.1';
const PORT = 4000;

const socket = new net.Socket();

enum Instruction {
  BUILD = 'build_machine',
}

socket.on('data', (data: any) => {
  console.log(data);
});

export function sendToRacket<T extends object>(
  data: T,
  instruction: Instruction,
) {
  const jsonObj = JSON.stringify({ instr: instruction, data: data });
  socket.connect(PORT, HOST, () => {
    const res = socket.write(`${jsonObj}\n`);
    console.log(res);
  });
}

sendToRacket({ msg: ['Hellow for Typescript'] }, Instruction.BUILD);
