export type MachineType = 'dfa' | 'ndfa' | 'pda' | 'tm' | 'tm-lang-rec';

type State = string;
export type FSMRule = DfaNdfaRule | PdaRule | TmMttmRule;
export type DfaNdfaRule = { start: State; input: string; end: State };
export type PdaRule = {
  start: State;
  input: string;
  startStack: string[];
  end: State;
  endStack: string[];
};

export type TmMttmRule = {
  start: State;
  startTape: string[];
  end: State;
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
