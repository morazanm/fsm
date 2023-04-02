export type MachineType = 'dfa' | 'ndfa' | 'pda' | 'tm' | 'tm-lang-rec';

export type StateName = string;
export type StateType = 'start' | 'final' | 'startFinal' | 'normal' | 'accept'
export type State = {
  name: StateName;
  type: StateType;
}


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
