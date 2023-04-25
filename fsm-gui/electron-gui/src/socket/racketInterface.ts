import { FSMInterfaceResponse } from '../types/machine';
import { FSMInterfacePayload } from '../types/machine';

export function sendMachinePayload(
  machine: FSMInterfacePayload,
): Promise<FSMInterfaceResponse> {
  return Promise.reject();
}
