import { ruleToString } from '../types/machine';
import { MachineState } from '../view/MainView';
import { writeFileSync } from 'fs';

export function saveMachine(filepath: string, machine: MachineState): boolean {
  const date = new Date(Date.now()).toString();
  const headMsg = `;; Generated by FSM-GUI 2.0 on ${date}\n`;
  const machineName = `m-${Math.floor(Math.random() * 3000)}`;
  const dataToWrite =
    headMsg + `(define ${machineName} ${machineToString(machine)})`;

  try {
    writeFileSync(filepath, dataToWrite, { flag: 'a' });
    return true;
  } catch (error) {
    return false;
  }
}

function machineToString(machine: MachineState): string {
  switch (machine.type) {
    case 'dfa':
      return dfaNdfaToString(machine);
    case 'ndfa':
      return dfaNdfaToString(machine);
    case 'pda':
      return pdaToString(machine);
    case 'tm':
      return tmMttmToString(machine);
    case 'tm-language-recognizer':
      return tmMttmToString(machine);
    default:
      throw Error('Invalid Machine Type');
  }
}

export function dfaNdfaToString(machine: MachineState): string {
  const states = `'(${machine.states.map((s) => s.name).join(' ')})`;
  const alpha = `'(${machine.alphabet.join(' ')})`;
  const start = `'${
    machine.states.find((s) => s.type === 'start' || s.type === 'startFinal')
      .name ?? ''
  }`;
  const finals = `'(${machine.states
    .filter((s) => s.type === 'final' || s.type === 'startFinal')
    .map((s) => s.name)
    .join(' ')})`;
  const rules = `'(${machine.rules.map((r) => ruleToString(r)).join(' ')})`;
  const noDead = machine.nodead ? "'no-dead" : '';
  return `(make-${machine.type}\n\t${states}\n\t${alpha}\n\t${start}\n\t${finals}\n\t${rules}\n\t${noDead})`;
}

export function pdaToString(machine: MachineState): string {
  const states = `'(${machine.states.map((s) => s.name).join(' ')})`;
  const alpha = `'(${machine.alphabet.join(' ')})`;
  const start = `'${
    machine.states.find((s) => s.type === 'start' || s.type === 'startFinal')
      .name ?? ''
  }`;
  const finals = `'(${machine.states
    .filter((s) => s.type === 'final' || s.type === 'startFinal')
    .map((s) => s.name)
    .join(' ')})`;
  const rules = `'(${machine.rules.map((r) => ruleToString(r)).join(' ')})`;
  const stack = `'(${machine.stackAlpha.join(' ')})`;
  return `(make-pda\n\t${states}\n\t${alpha}\n\t${stack}\n\t${start}\n\t${finals}\n\t${rules})`;
}

export function tmMttmToString(machine: MachineState): string {
  const states = `'(${machine.states.map((s) => s.name).join(' ')})`;
  const alpha = `'(${machine.alphabet.join(' ')})`;
  const start = `'${
    machine.states.find((s) => s.type === 'start' || s.type === 'startFinal')
      .name ?? ''
  }`;
  const finals = `'(${machine.states
    .filter(
      (s) =>
        s.type === 'final' || s.type === 'startFinal' || s.type === 'accept',
    )
    .map((s) => s.name)
    .join(' ')})`;
  const rules = `'(${machine.rules.map((r) => ruleToString(r)).join(' ')})`;
  const accept = machine.accept ? `\n\t'${machine.accept.name}` : '';

  return `(make-tm\n\t${states}\n\t${alpha}\n\t${rules}\n\t${start}\n\t${finals}${accept})`;
}
