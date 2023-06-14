import { useEffect, useRef, forwardRef } from 'react';
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

type RuleBoxProps = {
  idx: number;
  rule: FSMRule;
  isCurrent: boolean;
  singleElement: boolean;
  len: number;
};

const RuleBox = forwardRef(
  (
    { idx, rule, isCurrent, singleElement, len }: RuleBoxProps,
    ref: React.ForwardedRef<HTMLDivElement>,
  ) => {
    const theme = useTheme();
    const styleFirst = { marginLeft: 'auto' };
    const styleLast = { marginRight: 'auto' };
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
    // reasoning behind this logic: https://bhch.github.io/posts/2021/04/centring-flex-items-and-allowing-overflow-scroll/
    const setSpecialStyle = (idx: number, len: number) => {
      let res = {};
      if (idx === 0) {
        res = { ...res, ...styleFirst };
      }
      if (idx === len - 1) {
        res = { ...res, ...styleLast };
      }
      return res;
    };
    const tmpStyle = setSpecialStyle(idx, len);
    if (isCurrent) {
      return (
        <CurrentItem
          key={strRule}
          style={{ ...activeStyle, ...tmpStyle }}
          theme={theme}
          ref={ref}
        >
          <p>{strRule}</p>
        </CurrentItem>
      );
    } else {
      return (
        <Item
          key={strRule}
          style={{ ...activeStyle, ...tmpStyle }}
          theme={theme}
        >
          <p>{strRule}</p>
        </Item>
      );
    }
  },
);

const RuleComponent = (props: RuleComponentProps) => {
  const hasOneEle = props.rules.length === 1;
  const myRef = useRef(null);

  const executeScroll = () => myRef.current.scrollIntoView();

  useEffect(() => {
    if (myRef.current) {
      executeScroll();
    }
  });

  return (
    <Stack direction="row" display="flex" overflow="auto">
      {props.rules.map((rule, i) => (
        <RuleBox
          key={i}
          idx={i}
          len={props.rules.length}
          rule={rule}
          ref={myRef}
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
