import { FSMTransition } from '../types/machine';

export type BuildMachineResponse = {
  transitions: FSMTransition[];
  // Sometimes fsm-core will add the dead-state (ds). Because of this we
  // will return the result of sm-states. Any new states should be added to
  // the gui
  states: string[];
};
