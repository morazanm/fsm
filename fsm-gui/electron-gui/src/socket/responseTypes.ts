import {
  FSMAlpha,
  FSMRule,
  FSMTransition,
  MachineType,
  State,
} from '../types/machine';

export type BuildMachineResponse = {
  transitions: FSMTransition[];
  // Sometimes fsm-core will add the dead-state (ds). Because of this we
  // will return the result of sm-states. Any new states + rules should
  //  be added to the gui.
  states: State[];
  rules: FSMRule[];
};

export type PrebuiltMachineResponse = {
  states: State[];
  alpha: FSMAlpha[];
  rules: FSMRule[];
  type: MachineType;
};
