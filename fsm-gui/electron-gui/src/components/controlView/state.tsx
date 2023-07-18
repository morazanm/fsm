import { useEffect, useRef, useState } from 'react';
import {
  FSMAlpha,
  FSMRule,
  FSMTransition,
  MachineType,
  RacketInvariantFunc,
  State,
  StateName,
  StateType,
} from '../../types/machine';
import styled, { keyframes } from 'styled-components';
import { StateModal } from './StateModal';
import styles from './state.module.css';
import { MachineState } from '../../view/MainView';

const fadeIn = (transform: string) => keyframes`
from {
 transform: translateX(0px);
}

to {
  transform: ${transform};
}
`;

const Transform = styled.div<{ trans: string }>`
  animation: ${(p) => fadeIn(p.trans)} 0.5s linear forwards;
  cursor: pointer;
`;

const DontTransform = styled.div<{ trans: string }>`
  transform: ${(p) => p.trans};
  cursor: pointer;
`;

// Draws the inner circle if needed. The rotation is needed to
// make sure that the text inside is upright
const InnerCircle = ({
  name,
  type,
  rotate,
}: {
  name: StateName;
  type: StateType;
  rotate: number;
}) => {
  const counterRotate = `${rotate * -1}deg`;
  const getCssClass = () => {
    switch (type) {
      case 'final':
        return 'innerRed' as const;
      case 'startFinal':
        return 'innerRed' as const;
      default:
        return undefined;
    }
  };
  const cssClass = getCssClass();
  return cssClass ? (
    <div className={styles[cssClass]}>
      <span style={{ rotate: counterRotate }}>{name}</span>
    </div>
  ) : (
    <span style={{ rotate: counterRotate }}>{name}</span>
  );
};

type StateProps = {
  state: State;
  states: State[];
  rules: FSMRule[];
  rotate?: number;
  onSubmit: (msg: string) => void;
  machineType: MachineType;
  currentTransition: FSMTransition | undefined;
  resetMachineAndSet: (machine: Partial<MachineState>) => void;
  consumedInput: FSMAlpha[] | undefined;
  transform?: string;
  isConnectedToBackend: boolean;
  updateInvariant: (stateName: string, invFun: RacketInvariantFunc) => void;
};

const State = (props: StateProps) => {
  const [open, setOpen] = useState(false);
  const firstRender = useRef(true);
  const rotation = props.rotate ?? 0;

  useEffect(() => {
    firstRender.current = false;
  }, []);

  return (
    <>
      {' '}
      {firstRender.current ? (
        <Transform
          trans={props.transform}
          className={styles[props.state.type]}
          style={{ rotate: `${rotation}deg` }}
          onClick={() => setOpen(true)}
        >
          <InnerCircle
            name={props.state.name}
            type={props.state.type}
            rotate={rotation}
          />
        </Transform>
      ) : (
        <DontTransform
          trans={props.transform}
          className={styles[props.state.type]}
          style={{ rotate: `${rotation}deg` }}
          onClick={() => setOpen(true)}
        >
          <InnerCircle
            name={props.state.name}
            type={props.state.type}
            rotate={rotation}
          />
        </DontTransform>
      )}
      {open && <StateModal
        open={open}
        onClose={() => setOpen(false)}
        state={props.state}
        rules={props.rules}
        currentTransition={props.currentTransition}
        consumedInput={props.consumedInput}
        onSubmit={props.onSubmit}
        machineType={props.machineType}
        resetMachineAndSet={props.resetMachineAndSet}
        states={props.states}
        isConnectedToBackend={props.isConnectedToBackend}
        updateInvariant={props.updateInvariant}
      />}
    </>
  );
};

export default State;
