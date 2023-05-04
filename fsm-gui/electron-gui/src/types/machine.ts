// A list of numbers or lowercase symbols with a string representation of length
// one not including EMP
export type FSMAlpha = string;

// State: An uppercase letter (e.g., A) or a symbol comprised of an uppercase letter,
// dash, and number (e.g., A-72431).
export type StateName = string;

// string representation of a racket invariant function
export type RacketInvariantFunc = string;

// The supported types of machines that the GUI can handle
export type MachineType = 'dfa' | 'ndfa' | 'pda' | 'tm' | 'tm-lang-rec';

// The types of states can be displayed in the GUI
export type StateType = 'start' | 'final' | 'startFinal' | 'normal' | 'accept';

// Object representation of a single FSM state. The invariant function is optional
// since not all states are required to have one
export type State = {
  name: StateName;
  type: StateType;
  invFunc?: RacketInvariantFunc;
};

// Supported rule types
export type FSMRule = DfaNdfaRule | PdaRule | TmMttmRule;

// Object representation of a dfa/ndfa rule
export type DfaNdfaRule = { start: StateName; input: string; end: StateName };

// Object representation of a pda rule
export type PdaRule = {
  start: StateName;
  input: string;
  startStack: string[];
  end: StateName;
  endStack: string[];
};

// Object representation of a turing machine rule
// TODO: should mttm be in the name?
export type TmMttmRule = {
  start: StateName;
  startTape: string[];
  end: StateName;
  endTape: string[];
};

// A transition consists for a rule that is used and a boolean to represent
// if the invariant held. If the invariant is undefined then a invariant function
// was not supplied
export type Transition = {
  rule: FSMRule;
  invPass: boolean | null;
};

export type StartTransition = {
  start: StateName;
  invPass: boolean | null;
};

export type EndTransition = {
  end: StateName;
  action: 'accept' | 'reject';
  invPass: boolean | null;
};

export type FSMTransition = Transition | StartTransition | EndTransition;

// A object representation of a GUI machine. This holds all the values that are
// sent to the FSM racket interface for further processing by fsm-core + fsm-gui
export type FSMInterfacePayload = {
  states: State[];
  alphabet: FSMAlpha[];
  rules: FSMRule[];
  type: MachineType;
  input: FSMAlpha[];
};

/*
 * Helper Functions below
 */

export const isNormalTransition = (
  transition: FSMTransition,
): transition is Transition => {
  return (transition as Transition).rule !== undefined;
};

export const isStartTransition = (
  transition: FSMTransition,
): transition is StartTransition => {
  return (transition as StartTransition).start !== undefined;
};

export const isEndTransition = (
  transition: FSMTransition,
): transition is EndTransition => {
  return (transition as EndTransition).end !== undefined;
};

// returns true if the machine is a type of turing machine
export const isTmType = (type: MachineType): boolean =>
  type == 'tm' || type === 'tm-lang-rec';

// return true if the given rule is a type of turing machine rule
export const isTmTmLangRecRule = (rule: FSMRule): rule is TmMttmRule => {
  return (rule as TmMttmRule).startTape !== undefined;
};

// return true if the given rule is a pda rule
export const isPdaRule = (rule: FSMRule): rule is PdaRule => {
  return (rule as PdaRule).endStack !== undefined;
};

// return true if the given rule is a ndfa or dfa rule
export const isDfaNdfaRule = (rule: FSMRule): rule is DfaNdfaRule => {
  return (
    !isTmTmLangRecRule(rule) &&
    !isPdaRule(rule) &&
    (rule as DfaNdfaRule).start !== undefined
  );
};

// Given two rules, determines if the rules are equivalent to each other
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

// Returns the string representation for a rule
export const ruleToString = (rule: FSMRule): string => {
  if (isDfaNdfaRule(rule)) {
    return `(${rule.start}, ${rule.input}, ${rule.end})`;
  } else if (isPdaRule(rule)) {
    return 'TODO: finish';
  } else if (isTmTmLangRecRule(rule)) {
    return 'TODO: finish';
  }
  throw Error('Invalid rule supplied');
};
