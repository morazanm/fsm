export type MachineType = 'dfa' | 'ndfa' | 'pda' | 'tm' | 'tm-lang-rec';

export type StateName = string;
export type StateType = 'start' | 'final' | 'startFinal' | 'normal' | 'accept';
export type State = {
  name: StateName;
  type: StateType;
  //TODO: Add the invariant function
};

// A list of numbers or lowercase symbols with a string representation of length one not including EMP
export type FSMAlpha = string;
export type FSMRule = DfaNdfaRule | PdaRule | TmMttmRule;
export type DfaNdfaRule = { start: StateName; input: string; end: StateName };
export type PdaRule = {
  start: StateName;
  input: string;
  startStack: string[];
  end: StateName;
  endStack: string[];
};

export type TmMttmRule = {
  start: StateName;
  startTape: string[];
  end: StateName;
  endTape: string[];
};

export const isTmTmLangRecRule = (rule: FSMRule): rule is TmMttmRule => {
  return (rule as TmMttmRule).startTape !== undefined;
};

export const isPdaRule = (rule: FSMRule): rule is PdaRule => {
  return (rule as PdaRule).endStack !== undefined;
};

export const isDfaNdfaRule = (rule: FSMRule): rule is DfaNdfaRule => {
  return (
    !isTmTmLangRecRule(rule) &&
    !isPdaRule(rule) &&
    (rule as DfaNdfaRule).start !== undefined
  );
};

export const isFSMRuleEqual = (r1: FSMRule, r2: FSMRule): boolean => {
  const checkValsEqual = <T extends object>(obj1: T, obj2: T) =>
    Object.keys(obj1).reduce(
      (acc, key) => acc && obj1[key as keyof T] === obj2[key as keyof T],
      true,
    );
  if (isDfaNdfaRule(r1) && isDfaNdfaRule(r2)) {
    return checkValsEqual(r1, r2);
  } else if (isPdaRule(r1) && isPdaRule(r2)) {
    return checkValsEqual(r1, r2);
  } else if (isTmTmLangRecRule(r1) && isTmTmLangRecRule(r2)) {
    return checkValsEqual(r1, r2);
  } else {
    return false;
  }
};

export const ruleToString = (rule: FSMRule): string => {
  if (isDfaNdfaRule(rule)) {
    return `(${rule.start}, ${rule.input}, ${rule.end})`;
  } else if (isPdaRule(rule)) {
    return 'TODO';
  } else if (isTmTmLangRecRule(rule)) {
    return 'TODO';
  }
  throw Error('Invalid rule supplied');
};
