import { useState, useRef, useEffect } from 'react';
import { Box } from '@mui/material';
import { State, FSMRule } from '../../types/machine';
import StateComponent from './state';
import styles from './view.module.css';

type ControlViewProps = {
  states: State[];
  currentRule: FSMRule | undefined;
};

const ControlView = (props: ControlViewProps) => {
  const [height, setHeight] = useState(0);
  const ref = useRef(null);

  // In order to create the to and from arrows we render a transpartent circle over the
  // controlview circle. Then add the arrow and rotate the entire circle to the approperate
  // state (target)
  const Arrow = ({
    target,
    isCurrent,
  }: {
    target: string;
    isCurrent?: boolean;
  }) => {
    const style = isCurrent ? 'currentArrow' : 'previousArrow';
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
        {props.currentRule && (
          <>
            <Arrow target={props.currentRule.end} isCurrent />
            <Arrow target={props.currentRule.start} />
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
