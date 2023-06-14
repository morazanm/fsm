import {
  FSMAlpha,
  FSMRule,
  FSMStackAlpha,
  FSMTransition,
  MachineType,
  State,
} from '../types/machine';

export type BuildMachineResponse = {
  transitions: FSMTransition[];
  // Sometimes fsm-core will add the dead-state (ds). Because of this we
  // will return the result of sm-states. Any new states + rules should
  // be added to the gui.
  states: State[];
  rules: FSMRule[];
};

export type PrebuiltMachineResponse = {
  states: State[];
  alpha: FSMAlpha[];
  rules: FSMRule[];
  type: MachineType;
  stackAlpha: FSMStackAlpha[] | undefined; // undefined when not a pda

  // The absolute path to the graphViz image. If this field does not exist then
  // that means that graphViz is not on the users computer
  filepath: string | null;
};

export type RedrawnGraphvizImageResponse = {
  // The absolute path to the graphViz image. If this field does not exist then
  // that means that graphViz is not on the users computer
  filepath: string | null;
};
