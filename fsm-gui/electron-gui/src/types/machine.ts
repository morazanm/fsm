// A list of numbers or lowercase symbols with a string representation of length
// one not including EMP
export type FSMAlpha = string;

// A list of numbers or lowercase symbols with a string representation of length
// one not including EMP
export type FSMStackAlpha = string;

// State: An uppercase letter (e.g., A) or a symbol comprised of an uppercase letter,
// dash, and number (e.g., A-72431).
export type StateName = string;

// string representation of a racket invariant function
export type RacketInvariantFunc = string;

// The supported types of machines that the GUI can handle
export type MachineType =
  | 'dfa'
  | 'ndfa'
  | 'pda'
  | 'tm'
  | 'tm-language-recognizer';

// The types of states can be displayed in the GUI
export type StateType = 'start' | 'final' | 'startFinal' | 'normal' | 'accept';

// Object representation of a single FSM state. The invariant function is optional
// since not all states are required to have one
export type State = {
  name: StateName;
  type: StateType;
  invFunc: RacketInvariantFunc | null;
};

// Supported rule types
export type FSMRule = DfaNdfaRule | PdaRule | TmMttmRule;

// Object representation of a dfa/ndfa rule
export type DfaNdfaRule = { start: StateName; input: string; end: StateName };

// Object representation of a pda rule
export type PdaRule = {
  start: StateName;
  input: string;
  popped: string[]; // empty array is displayed as EMP
  end: StateName;
  pushed: string[]; // empty array is displayed as EMP
};

type TmAction = 'R' | 'L' | '_' | '@';
// Object representation of a turing machine rule
export type TmMttmRule = {
  start: StateName;
  startTape: string[] | TmAction;
  end: StateName;
  endTape: string[] | TmAction;
};

// A BasicTransition is used in all types of machines. It consists of a rule that is used
// and a boolean to represent if the invariant held for the transition. If the invariant
// is null then a invariant function was not supplied by the user.
export type BasicTransition = {
  rule: FSMRule;
  invPass: boolean | null;

  // When the filepath key does not exist then that means that graphViz is not installed
  // on the users computer
  filepath?: string;
};

// A PdaTransition also includes what the stack holds at the transition
export type PdaTransition = BasicTransition & {
  stack: FSMStackAlpha[];
};

// A TmMttmTransition also includes the index of the current place on the tape that the
// turing machine is at and the new tape after the transition
export type TmMttmTransition = BasicTransition & {
  tapeIndex: number;
  tape: FSMAlpha[];
};

// StartTransition is the initial transition for the machine. It just contains a invariant and
// a state state.
export type StartTransition = {
  start: StateName;
  invPass: boolean | null;

  // When the filepath key does not exist then that means that graphViz is not installed
  // on the users computer
  filepath?: string;
};

export type TmStartTransition = StartTransition & {
  tapeIndex: number;
  tape: FSMAlpha[];
};

// EndTransition is the last transition for the machine.  It either accepts or rejects the input.
export type EndTransition = {
  end: StateName;
  action: 'accept' | 'reject';
  invPass: boolean | null;

  // When the filepath key does not exist then that means that graphViz is not installed
  // on the users computer
  filepath?: string;
};

export type TmEndTransition = EndTransition & {
  tapeIndex: number;
  tape: FSMAlpha[];
};

export type FSMTransition =
  | BasicTransition
  | PdaTransition
  | TmMttmTransition
  | StartTransition
  | EndTransition
  | TmStartTransition
  | TmEndTransition;

/*
 * Helper Functions below
 */

export const isPdaTransition = (
  transition: FSMTransition,
): transition is PdaTransition => {
  return (transition as PdaTransition).stack !== undefined;
};

export const isTmMttmTransition = (
  transition: FSMTransition,
): transition is TmMttmTransition => {
  return (transition as TmMttmTransition).tapeIndex !== undefined;
};

export const isNormalTransition = (
  transition: FSMTransition,
): transition is BasicTransition => {
  return (transition as BasicTransition).rule !== undefined;
};

export const isTmStartTransition = (
  transition: FSMTransition,
): transition is TmStartTransition => {
  return (
    isStartTransition(transition) &&
    (transition as TmStartTransition).tapeIndex != undefined
  );
};

export const isTmEndTransition = (
  transition: FSMTransition,
): transition is TmEndTransition => {
  return (
    isEndTransition(transition) &&
    (transition as TmEndTransition).tapeIndex != undefined
  );
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
  type == 'tm' || type === 'tm-language-recognizer';

// return true if the given rule is a type of turing machine rule
export const isTmTmLangRecRule = (rule: FSMRule): rule is TmMttmRule => {
  return (rule as TmMttmRule).startTape !== undefined;
};

// return true if the given rule is a pda rule
export const isPdaRule = (rule: FSMRule): rule is PdaRule => {
  return (rule as PdaRule).popped !== undefined;
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
  const cmpArrays = <T>(a1: T[], a2: T[]) =>
    a1.length === a2.length && a1.every((v, i) => v === a2[i]);

  if (isDfaNdfaRule(r1) && isDfaNdfaRule(r2)) {
    return r1.start === r2.start && r1.end === r2.end && r1.input === r2.input;
  } else if (isPdaRule(r1) && isPdaRule(r2)) {
    return (
      r1.start === r2.start &&
      r1.end === r2.end &&
      cmpArrays(r1.popped, r2.popped) &&
      cmpArrays(r1.pushed, r2.pushed)
    );
  } else if (isTmTmLangRecRule(r1) && isTmTmLangRecRule(r2)) {
    const tmp1 =
      Array.isArray(r1.startTape) && Array.isArray(r2.startTape)
        ? r1.startTape[0] === r2.startTape[0]
        : r1.startTape === r2.startTape;
    const tmp2 =
      Array.isArray(r1.endTape) && Array.isArray(r2.endTape)
        ? r1.endTape[0] === r2.endTape[0]
        : r1.endTape === r2.endTape;
    return r1.start === r2.start && r1.end === r2.end && tmp1 && tmp2;
  } else {
    return false;
  }
};

// Returns the string representation for a rule
export const ruleToString = (rule: FSMRule): string => {
  const arrayToString = (array: string[]) => {
    return array.length === 0 ? 'ε' : `(${array.join(' ')})`;
  };
  if (isDfaNdfaRule(rule)) {
    return `(${rule.start} ${rule.input} ${rule.end})`;
  } else if (isPdaRule(rule)) {
    return `((${rule.start} ${rule.input} ${arrayToString(rule.popped)}) (${
      rule.end
    } ${arrayToString(rule.pushed)}))`;
  } else if (isTmTmLangRecRule(rule)) {
    return `((${rule.start} ${rule.startTape}) (${rule.end} ${rule.endTape}))`;
  }
  throw Error('Invalid rule supplied');
};
