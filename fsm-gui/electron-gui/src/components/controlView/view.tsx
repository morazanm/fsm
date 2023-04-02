import { useState, useRef, useEffect } from 'react';
import { State } from '../../types/machine';
import StateComponent from './state';
import styles from './view.module.css';

type ControlViewProps = {
  states: State[];
};

const ControlView = (props: ControlViewProps) => {
  const [height, setHeight] = useState(0);
  const ref = useRef(null);

  useEffect(() => {
    const handleResize = () => {
      setHeight(ref.current.clientHeight);
    };
    handleResize();
    window.addEventListener('resize', handleResize);
  });

  return (
    <div ref={ref} className={styles.main}>
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
  );
};

export default ControlView;
