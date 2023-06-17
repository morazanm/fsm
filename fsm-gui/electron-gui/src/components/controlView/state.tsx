import { StateName, StateType } from '../../types/machine';
import styles from './state.module.css';

type StateProps = {
  stateName: StateName;
  stateType: StateType;
  rotate?: number;
  style?: React.CSSProperties;
};

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

const State = (props: StateProps) => {
  const rotation = props.rotate ?? 0;
  return (
    <div
      className={styles[props.stateType]}
      style={{ ...props.style, rotate: `${rotation}deg` }}
      onClick={() => console.log(`${props.stateName} clicked`)}
    >
      <InnerCircle
        name={props.stateName}
        type={props.stateType}
        rotate={rotation}
      />
    </div>
  );
};

export default State;
