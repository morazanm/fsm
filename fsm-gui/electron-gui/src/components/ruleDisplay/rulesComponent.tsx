import { isFSMRuleEqual, ruleToString } from '../../types/machine';
import { FSMRule } from '../../types/machine';
import { Stack, Divider, Paper } from '@mui/material';
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

const RuleBox = ({
  rule,
  isCurrent,
}: {
  rule: FSMRule;
  isCurrent: boolean;
}) => {
  const strRule = ruleToString(rule);
  if (isCurrent) {
    return (
      <CurrentItem key={strRule} style={{ minWidth: 'fit-content', display: "flex", alignItems: "center"}}>
        <p>{strRule}</p>
      </CurrentItem>
    );
  } else {
    return (
      <Item key={strRule} style={{ minWidth: 'fit-content', display: "flex", alignItems: "center" }}>
        <p>{strRule}</p>
      </Item>
    );
  }
};

const RuleComponent = (props: RuleComponentProps) => {
  return (
    <Stack
      direction="row"
      overflow="auto"
      divider={<Divider orientation="vertical" flexItem />}
    >
      {props.rules.map((rule, i) => (
        <RuleBox
          rule={rule}
          isCurrent={isFSMRuleEqual(rule, props.currentRule)}
        />
      ))}
    </Stack>
  );
};

export default RuleComponent;
