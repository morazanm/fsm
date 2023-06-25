import { useState, useRef, useEffect } from 'react';
import { Alert, Box, Snackbar, useTheme } from '@mui/material';
import {
  State,
  FSMTransition,
  isNormalTransition,
  StateName,
  isStartTransition,
  FSMAlpha,
  isDfaNdfaRule,
  isPdaRule,
  FSMRule,
  MachineType,
} from '../../types/machine';
import StateComponent from './state';
import styles from './view.module.css';
import { MachineState } from '../../view/MainView';
import { styled } from 'styled-components';

const StyledCurrentArrow = styled.hr<{ color: string }>`
  &:after {
    content: '';
    width: 0;
    height: 0;
    border-top: 20px solid transparent;
    border-bottom: 20px solid transparent;
    border-left: 30px solid ${(p) => p.color};
    position: absolute;
    position: fixed;
    display: inline-block;
    right: 8px;
    top: 15px;
  }
`;

type ControlViewProps = {
  states: State[];
  rules: FSMRule[];
  machineType: MachineType;
  consumedInput: FSMAlpha[] | undefined;
  currentTransition: FSMTransition | null;
  resetMachineAndSet: (machine: Partial<MachineState>) => void;
  isConnectedToBackend: boolean;
};

type CurrentTransition = {
  start: StateName | null;
  end: StateName | null;
  invPass: boolean | null;
  input: FSMAlpha | null;
};

const getCurrentStateAndInv = (
  trans: FSMTransition | null,
): CurrentTransition | null => {
  if (!trans) return null;
  if (isNormalTransition(trans))
    return {
      start: trans.rule.start,
      end: trans.rule.end,
      invPass: trans.invPass,
      input: isDfaNdfaRule(trans.rule)
        ? trans.rule.input
        : isPdaRule(trans.rule)
        ? trans.rule.input
        : null,
    };
  else if (isStartTransition(trans))
    return {
      start: null,
      end: trans.start,
      invPass: trans.invPass,
      input: null,
    };
  else {
    return { start: null, end: trans.end, invPass: trans.invPass, input: null };
  }
};

const ControlView = (props: ControlViewProps) => {
  const theme = useTheme();
  const [snack, setSnack] = useState({ open: false, msg: '' });
  const [height, setHeight] = useState(0);
  const ref = useRef(null);
  // In order to create the to and from arrows we render a transparent circle over the
  // control-view circle. Then add the arrow and rotate the entire circle to the appropriate
  // state (target)
  const Arrow = ({
    target,
    isCurrent,
    invPass,
    input,
  }: {
    target: string;
    isCurrent?: boolean;
    invPass?: boolean | null;
    input?: string;
  }) => {
    const style = isCurrent ? 'currentArrow' : 'previousArrow';
    const arrowColor =
      invPass === null || invPass === undefined
        ? theme.palette.primary.main
        : invPass
        ? theme.palette.success.main
        : theme.palette.error.main;
    const arrowStyle = style === 'currentArrow' ? 'solid' : 'dotted';
    const stateIndex = props.states.findIndex((s) => s.name === target);
    if (stateIndex !== -1) {
      const rotation = (360 / props.states.length) * stateIndex;
      const center = height / 2 - 5;
      const useBottom = rotation > 90 && rotation < 270;
      return (
        <div
          className={styles.circleBackground}
          style={{
            rotate: `${rotation}deg`,
            height: `${height}px`,
            width: `${height}px`,
          }}
        >
          <div
            style={{
              transform: `translateY(${center - 60}px) translateX(${center}px)`,
              width: `${height / 2 - 30}px`,
              height: '60px',
              textAlign: 'center',
            }}
          >
            {style === 'previousArrow' ? (
              <hr
                className={styles[style]}
                style={{
                  borderTop: `10px ${theme.palette.text.disabled} ${arrowStyle}`,
                  width: `${height / 2 - 40}px`,
                }}
              ></hr>
            ) : (
              <StyledCurrentArrow
                color={arrowColor}
                className={styles[style]}
                style={{
                  borderTop: `10px ${arrowColor} ${arrowStyle}`,
                  width: `${height / 2 - 60}px`,
                }}
              ></StyledCurrentArrow>
            )}
            <p
              style={{ rotate: `${useBottom ? 180 : 0}deg`, fontSize: '30px' }}
            >
              {input ? input : ''}
            </p>
          </div>
        </div>
      );
    }
    return <div></div>;
  };

  useEffect(() => {
    const handleResize = () => {
      setHeight(ref.current.clientHeight - 100);
    };
    handleResize();
    window.addEventListener('resize', handleResize);
    return () => {
      window.removeEventListener('resize', handleResize);
    };
  });

  const currentTrans = getCurrentStateAndInv(props.currentTransition);

  return (
    <Box
      ref={ref}
      justifyContent="center"
      alignItems="center"
      display="flex"
      style={{ height: '70vh', width: '70vh' }}
    >
      <div
        className={styles.circleMain}
        style={{ height: `${height}px`, width: `${height}px` }}
      >
        {' '}
        {currentTrans && (
          <>
            {currentTrans.end && (
              <Arrow
                target={currentTrans.end}
                invPass={currentTrans.invPass}
                input={currentTrans.input}
                isCurrent
              />
            )}
            {currentTrans.start && <Arrow target={currentTrans.start} />}
          </>
        )}
        {props.states.map((state, i) => (
          <StateComponent
            key={i}
            state={state}
            states={props.states}
            rules={props.rules}
            currentTransition={props.currentTransition}
            consumedInput={props.consumedInput}
            onSubmit={(msg: string) => {
              setSnack({ open: true, msg });
            }}
            machineType={props.machineType}
            resetMachineAndSet={props.resetMachineAndSet}
            rotate={(360 / props.states.length) * i}
            transform={`translateX(${height / 2}px)`}
            isConnectedToBackend={props.isConnectedToBackend}
          />
        ))}
      </div>
      <Snackbar
        open={snack.open}
        autoHideDuration={6000}
        onClose={() => setSnack({ open: false, msg: '' })}
      >
        <Alert
          onClose={() => setSnack({ open: false, msg: '' })}
          severity="success"
          sx={{ width: '100%' }}
        >
          {snack.msg}
        </Alert>
      </Snackbar>
    </Box>
  );
};

export default ControlView;
