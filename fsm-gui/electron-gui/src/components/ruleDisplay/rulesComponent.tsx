import {
  FSMTransition,
  isFSMRuleEqual,
  isNormalTransition,
  ruleToString,
} from '../../types/machine';
import { FSMRule } from '../../types/machine';
import { Stack, Paper } from '@mui/material';
import { styled, useTheme } from '@mui/material/styles';

const Item = styled(Paper)(({ theme }) => ({
  backgroundColor: theme.palette.background.default,
  ...theme.typography.body2,
  padding: theme.spacing(1),
  textAlign: 'center',
  color: theme.palette.text.primary,
}));

const CurrentItem = styled(Paper)(({ theme }) => ({
  backgroundColor: theme.palette.background.default,
  ...theme.typography.body2,
  padding: theme.spacing(1),
  textAlign: 'center',
  color: theme.palette.info.main,
}));

type RuleComponentProps = {
  rules: FSMRule[];
  currentTransition: FSMTransition | undefined;
};

const RuleBox = ({
  idx,
  rule,
  isCurrent,
  singleElement,
}: {
  idx: number;
  rule: FSMRule;
  isCurrent: boolean;
  singleElement: boolean;
}) => {
  const theme = useTheme();
  const styleRight = {
    minWidth: 'fit-content',
    display: 'flex',
    alignItems: 'center',
    marginLeft: '-1px',
    borderLeft: `1px solid ${theme.palette.divider}`,
    borderRight: `1px solid ${theme.palette.divider}`,
    borderRadius: '0px',
    boxShadow: 'none',
  };

  const styleSingle = {
    minWidth: 'fit-content',
    display: 'flex',
    alignItems: 'center',
    borderLeft: `1px solid ${theme.palette.divider}`,
    borderRight: `1px solid ${theme.palette.divider}`,
    borderRadius: '0px',
    boxShadow: 'none',
  };

  const styleLeft = {
    minWidth: 'fit-content',
    display: 'flex',
    alignItems: 'center',
    borderLeft: `1px solid ${theme.palette.divider}`,
    borderRadius: '0px',
    boxShadow: 'none',
  };
  const activeStyle = singleElement
    ? styleSingle
    : idx === 0
    ? styleLeft
    : styleRight;

  const strRule = ruleToString(rule);
  if (isCurrent) {
    return (
      <CurrentItem key={strRule} style={activeStyle} theme={theme}>
        <p>{strRule}</p>
      </CurrentItem>
    );
  } else {
    return (
      <Item key={strRule} style={activeStyle} theme={theme}>
        <p>{strRule}</p>
      </Item>
    );
  }
};

const RuleComponent = (props: RuleComponentProps) => {
  const hasOneEle = props.rules.length === 1;
  return (
    <Stack
      direction="row"
      display="flex"
      overflow="auto"
      justifyContent="center"
    >
      {props.rules.map((rule, i) => (
        <RuleBox
          key={i}
          idx={i}
          rule={rule}
          singleElement={hasOneEle}
          isCurrent={
            props.currentTransition &&
            isNormalTransition(props.currentTransition) &&
            isFSMRuleEqual(rule, props.currentTransition.rule)
          }
        />
      ))}
    </Stack>
  );
};

export default RuleComponent;
