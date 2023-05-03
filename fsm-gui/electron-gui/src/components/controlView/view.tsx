import { useState, useRef, useEffect } from 'react';
import { Box, useTheme } from '@mui/material';
import {
  State,
  FSMTransition,
  isNormalTransition,
  StateName,
  isStartTransition,
} from '../../types/machine';
import StateComponent from './state';
import styles from './view.module.css';

type ControlViewProps = {
  states: State[];
  currentTransition: FSMTransition | null;
};

type CurrentTransition = {
  start: StateName | null;
  end: StateName | null;
  invPass: boolean | null;
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
    };
  else if (isStartTransition(trans))
    return { start: null, end: trans.start, invPass: trans.invPass };
  else {
    return { start: null, end: trans.end, invPass: trans.invPass };
  }
};

const ControlView = (props: ControlViewProps) => {
  const theme = useTheme();
  const [height, setHeight] = useState(0);
  const ref = useRef(null);
  // In order to create the to and from arrows we render a transparent circle over the
  // control-view circle. Then add the arrow and rotate the entire circle to the appropriate
  // state (target)
  const Arrow = ({
    target,
    isCurrent,
    invPass,
  }: {
    target: string;
    isCurrent?: boolean;
    invPass?: boolean | null;
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
      return (
        <div
          className={styles.circleBackground}
          style={{
            rotate: `${rotation}deg`,
            height: `${height}px`,
            width: `${height}px`,
          }}
        >
          <hr
            className={styles[style]}
            style={{
              borderTop: `10px ${
                style === 'previousArrow'
                  ? theme.palette.text.disabled
                  : arrowColor
              } ${arrowStyle}`,
              width: `${height / 2 - 20}px`,
              transform: `translateY(-5px) translateX(${center}px)`,
            }}
          ></hr>
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
                isCurrent
              />
            )}
            {currentTrans.start && <Arrow target={currentTrans.start} />}
          </>
        )}
        {props.states.map((state, i) => (
          <StateComponent
            key={i}
            stateName={state.name}
            stateType={state.type}
            rotate={(360 / props.states.length) * i}
            style={{ transform: `translateX(${height / 2}px)` }}
          />
        ))}
      </div>
    </Box>
  );
};

export default ControlView;
