import { Instruction, SocketResponse } from '../socket/racketInterface';
import { MachineState } from './MainView';
import {
  BuildMachineResponse,
  PrebuiltMachineResponse,
  RecomputeInvariantResponse,
  RedrawnGraphvizImageResponse,
} from '../socket/responseTypes';
import { isFSMRuleEqual, isTmType } from '../types/machine';

type ResultData = MachineState | InvUpdate | string;
type Result = {
  data: ResultData;
  instruction: Instruction;
};

type InvUpdate = {
  syntaxErrorMsg: string | undefined;
  hasFailingInv: boolean;
  updatedMachine: MachineState;
};

export function isInvUpdate(data: ResultData): data is InvUpdate {
  return (data as InvUpdate).updatedMachine !== undefined;
}

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
  } else if (result.responseType === Instruction.RECOMPUTE_INV) {
    const response = result as SocketResponse<RecomputeInvariantResponse>;
    const hasSyntaxError = response.data.changedStatuses.find(
      (s) => typeof s.status === 'string',
    );
    const hasFailingInvariants =
      hasSyntaxError ||
      response.data.changedStatuses.find((s) => s.status !== true);
    let newTrans = currentMachine.transitions.transitions;
    response.data.changedStatuses.forEach((v) => {
      newTrans[v.index].filepath = v.filepath;
      newTrans[v.index].invPass = v.status;
    });
    return {
      data: {
        syntaxErrorMsg:
          hasSyntaxError !== undefined &&
          typeof hasSyntaxError.status === 'string'
            ? hasSyntaxError.status
            : undefined,
        hasFailingInv: hasFailingInvariants !== undefined,
        updatedMachine: {
          ...currentMachine,
          transitions: {
            ...currentMachine.transitions,
            transitions: newTrans,
          },
        },
      } as InvUpdate,
      instruction: response.responseType,
    };
  } else {
    return {
      instruction: result.responseType,
      data: `Unable to parse response type: ${result.responseType}`,
    };
  }
}
