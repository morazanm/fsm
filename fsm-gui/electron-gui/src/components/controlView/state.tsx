import { useState } from 'react';
import {
  FSMAlpha,
  FSMRule,
  FSMTransition,
  MachineType,
  State,
  StateName,
  StateType,
} from '../../types/machine';
import { StateModal } from './StateModal';
import styles from './state.module.css';
import { MachineState } from '../../view/MainView';

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
  style?: React.CSSProperties;
};

const State = (props: StateProps) => {
  const [open, setOpen] = useState(false);
  const rotation = props.rotate ?? 0;
  return (
    <>
      <div
        className={styles[props.state.type]}
        style={{ ...props.style, rotate: `${rotation}deg`, cursor: 'pointer' }}
        onClick={() => setOpen(true)}
      >
        <InnerCircle
          name={props.state.name}
          type={props.state.type}
          rotate={rotation}
        />
      </div>
      <StateModal
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
      />
    </>
  );
};

export default State;
