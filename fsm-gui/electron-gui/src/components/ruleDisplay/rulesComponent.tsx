import { isFSMRuleEqual, ruleToString } from '../../types/machine';
import { FSMRule } from '../../types/machine';
import { Stack, Paper } from '@mui/material';
import { styled } from '@mui/material/styles';

const Item = styled(Paper)(({ theme }) => ({
  backgroundColor: theme.palette.mode === 'dark' ? '#1A2027' : '#fff',
  ...theme.typography.body2,
  padding: theme.spacing(1),
  textAlign: 'center',
  color: theme.palette.text.primary,
}));

const CurrentItem = styled(Paper)(({ theme }) => ({
  backgroundColor: theme.palette.mode === 'dark' ? '#1A2027' : '#fff',
  ...theme.typography.body2,
  padding: theme.spacing(1),
  textAlign: 'center',
  color: theme.palette.info.main,
}));

type RuleComponentProps = {
  rules: FSMRule[];
  currentRule: FSMRule;
};

const style = {
  minWidth: 'fit-content',
  display: 'flex',
  alignItems: 'center',
  marginLeft: '-1px',
  borderLeft: '1px solid var(--mui-palette-divider)',
  borderRight: '1px solid var(--mui-palette-divider)',
  borderRadius: '0px',
  boxShadow: 'none',
};

const styleLeft = {
  minWidth: 'fit-content',
  display: 'flex',
  alignItems: 'center',
  borderLeft: '1px solid var(--mui-palette-divider)',
  borderRadius: '0px',
  boxShadow: 'none',
};

const RuleBox = ({
  idx,
  rule,
  isCurrent,
}: {
  idx: number;
  rule: FSMRule;
  isCurrent: boolean;
}) => {
  const strRule = ruleToString(rule);
  if (isCurrent) {
    return (
      <CurrentItem key={strRule} style={idx === 0 ? styleLeft : style}>
        <p>{strRule}</p>
      </CurrentItem>
    );
  } else {
    return (
      <Item key={strRule} style={idx === 0 ? styleLeft : style}>
        <p>{strRule}</p>
      </Item>
    );
  }
};

const RuleComponent = (props: RuleComponentProps) => {
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
          isCurrent={isFSMRuleEqual(rule, props.currentRule)}
        />
      ))}
    </Stack>
  );
};

export default RuleComponent;
