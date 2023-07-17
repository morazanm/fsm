import { Instruction, SocketResponse } from '../socket/racketInterface';
import { MachineState } from './MainView';
import {
  BuildMachineResponse,
  PrebuiltMachineResponse,
  RedrawnGraphvizImageResponse,
} from '../socket/responseTypes';
import { isFSMRuleEqual, isTmType } from '../types/machine';

type Result = {
  data: MachineState | string;
  instruction: Instruction;
};

export function parseDataResponse(
  data: string,
  currentMachine: MachineState,
): Result {
  const result: SocketResponse<object> = JSON.parse(data);
  if (result.error) {
    return { data: `${result.error}`, instruction: result.responseType };
  } else if (result.responseType === Instruction.BUILD) {
    const response = result as SocketResponse<BuildMachineResponse>;

    // See if fsm-core added any states, if so then add them
    const new_states = currentMachine.states.concat(
      response.data.states.filter(
        (s) => !currentMachine.states.find((st) => st.name === s.name),
      ),
    );
    // See if fsm-core added any rules, if so then add them
    const new_rules = currentMachine.rules.concat(
      response.data.rules.filter(
        (r) => !currentMachine.rules.find((mr) => isFSMRuleEqual(r, mr)),
      ),
    );
    console.log(response.data.transitions);
    return {
      data: {
        ...currentMachine,
        states: new_states,
        rules: new_rules,
        transitions: {
          transitions: response.data.transitions,
          index: 0,
          inputIndex: -1,
        },
        stackAlpha:
          currentMachine.type === 'pda' ? currentMachine.stackAlpha : undefined,
      },
      instruction: result.responseType,
    };
  } else if (result.responseType === Instruction.PREBUILT) {
    const response = result as SocketResponse<PrebuiltMachineResponse>;
    console.log(response.data.states);
    return {
      data: {
        ...currentMachine,
        input: isTmType(response.data.type) ? ['@'] : [],
        states: response.data.states,
        alphabet: response.data.alpha,
        rules: response.data.rules,
        type: response.data.type,
        hotReload: response.data.hotReload,
        accept: response.data.states.find((s) => s.type === 'accept'),
        stackAlpha:
          response.data.type === 'pda' ? response.data.stackAlpha : [],
        graphVizImage: response.data.filepath ?? null,
      },
      instruction: response.responseType,
    };
  } else if (result.responseType === Instruction.REDRAW) {
    const response = result as SocketResponse<RedrawnGraphvizImageResponse>;
    return {
      instruction: response.responseType,
      data: response.data.filepath,
    };
  } else {
    return {
      instruction: result.responseType,
      data: `Unable to parse response type: ${result.responseType}`,
    };
  }
}
