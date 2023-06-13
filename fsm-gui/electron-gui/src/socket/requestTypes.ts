// A object representation of a GUI machine. This holds all the values that are

import {
  FSMAlpha,
  FSMRule,
  FSMStackAlpha,
  MachineType,
  State,
} from '../types/machine';

// send to the FSM racket interface for further processing by fsm-core + fsm-gui
export type FSMBuildMachineRequest = {
  states: State[];
  alphabet: FSMAlpha[];
  rules: FSMRule[];
  type: MachineType;
  input: FSMAlpha[];
  nodead: boolean;
  tapeIndex: number | undefined; // Only defined for TM's
  stackAlpha: FSMStackAlpha[] | undefined; // undefined when not a pda
};
