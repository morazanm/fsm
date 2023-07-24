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
  hotReload: boolean; //when set to true, invariants can be edited in the GUI

  // The absolute path to the graphViz image. If this field does not exist then
  // that means that graphViz is not on the users computer
  filepath: string | null;
};

export type RedrawnGraphvizImageResponse = {
  // The absolute path to the graphViz image. If this field does not exist then
  // that means that graphViz is not on the users computer
  filepath: string | null;
};

// Message from from racket for the invalid syntax
type InvalidSyntaxError = string;
type Filepath = string;

export type RecomputeInvariantResponse = {
  targetState: string;
  changedStatuses: {
    index: number;
    status: boolean | InvalidSyntaxError;
    filepath: Filepath;
  }[];
};
