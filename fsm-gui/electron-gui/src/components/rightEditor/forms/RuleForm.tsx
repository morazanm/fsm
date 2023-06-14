import { useState } from 'react';
import Stack from '@mui/material/Stack';
import DialogContent from '@mui/material/DialogContent';
import DialogContentText from '@mui/material/DialogContentText';
import FormControl from '@mui/material/FormControl';
import GenericForm from './GenericForm';
import {
  MachineType,
  State,
  FSMRule,
  FSMAlpha,
  isTmTmLangRecRule,
  isPdaRule,
  isDfaNdfaRule,
  isFSMRuleEqual,
  isTmType,
  TmMttmRule,
} from '../../../types/machine';
import { FormHelperText, Input, InputLabel } from '@mui/material';

function parseData(data: string, type: MachineType) {
  if (isTmType(type)) {
    const v = data.trim();
    if (v == 'BLANK') {
      return '_';
    } else if (v === 'LM') {
      return '@';
    } else if (v === 'LEFT') {
      return 'L';
    } else if (v === 'RIGHT') {
      return 'R';
    } else {
      return v;
    }
  } else {
    return data;
  }
}

function validateRule(
  rule: FSMRule,
  rules: FSMRule[],
  states: State[],
  alpha: string[],
  checkExists = true,
): string {
  const isValidState = (state: string): string =>
    states.find((s) => s.name === state)
      ? ''
      : `'${state}' is not currently a state`;

  const validAlpha = (a: FSMAlpha): boolean => alpha.includes(a) || a === 'ε';
  const isValidAlpha = (a: FSMAlpha) =>
    validAlpha(a) ? '' : `'${a}' is not in the alphabet`;
  const isValidStack = (s: FSMAlpha[]): string => {
    const inValidAlphas = [...new Set(s.filter((a) => !validAlpha(a)))];
    return inValidAlphas.length > 0
      ? `'${inValidAlphas}' are not in the alphabet`
      : '';
  };
  const fmtMsgs = (msgs: string[]) => msgs.filter((m) => m !== '').join('. ');

  if (Object.values(rule).filter((v) => v === '').length > 0) {
    return 'Please fill in all the rules inputs';
  }
  if (isTmTmLangRecRule(rule)) {
    //NOTE: All TM-Actions are valid
    return fmtMsgs([
      isValidState(rule.start),
      Array.isArray(rule.startTape) ? isValidAlpha(rule.startTape[0]) : '',
      isValidState(rule.end),
      Array.isArray(rule.endTape) ? isValidAlpha(rule.endTape[0]) : '',
    ]);
  } else if (isPdaRule(rule)) {
    rule.popped = rule.popped.filter((r) => r !== '');
    rule.pushed = rule.pushed.filter((r) => r !== '');
    if (checkExists && rules.find((r) => isFSMRuleEqual(r, rule))) {
      return 'Rule already exists in the list of rules';
    }
    return fmtMsgs([
      isValidState(rule.start),
      isValidAlpha(rule.input),
      isValidStack(rule.popped),
      isValidState(rule.end),
      isValidStack(rule.pushed),
    ]);
  } else if (isDfaNdfaRule(rule)) {
    if (checkExists && rules.find((r) => isFSMRuleEqual(r, rule))) {
      return 'Rule already exists in the list of rules';
    }
    return fmtMsgs([
      isValidAlpha(rule.input),
      isValidState(rule.start),
      isValidState(rule.end),
    ]);
  } else {
    throw Error(`Unable to determine rule type ${rule}`);
  }
}

function initRule(type: MachineType): FSMRule {
  switch (type) {
    case 'dfa':
      return { start: '', input: '', end: '' };
    case 'ndfa':
      return { start: '', input: '', end: '' };
    case 'pda':
      return { start: '', input: '', popped: [], end: '', pushed: [] };
    case 'tm':
      return { start: '', startTape: [], end: '', endTape: [] };
    case 'tm-language-recognizer':
      return { start: '', startTape: [], end: '', endTape: [] };
    default:
      console.log('OH NO', type);
  }
}

type RuleInputProps = {
  value: string;
  label: string;
  onChange: (val: string) => void;
  hasError: boolean;
};

const RuleInput = (props: RuleInputProps) => {
  const name = props.label
    .split(/(?=[A-Z])/)
    .map((word) => word.charAt(0).toUpperCase() + word.slice(1))
    .join(' ');

  return (
    <FormControl variant="standard" error={!!props.hasError}>
      <InputLabel>{name}</InputLabel>
      <Input
        value={props.value}
        onChange={(e) => props.onChange(e.target.value)}
      />
    </FormControl>
  );
};

const parseEMP = (val: string) => (val === 'EMP' ? 'ε' : val);

const parseValueAsArray = (value: string): string[] => {
  return value
    .split(' ')
    .filter((e) => e !== ' ')
    .map((e) => parseEMP(e));
};

const unParseValue = (values: string | string[]): string => {
  return Array.isArray(values) ? values.join(' ') : values;
};

const parseValueAsString = (value: string): string => {
  return parseEMP(value.trim());
};

type RuleFormProps = {
  isOpen: boolean;
  machineType: MachineType;
  toggle: () => void;
  toggleSnack: (msg: string) => void;
  rules: FSMRule[];
  setRules: (states: FSMRule[]) => void;
  states: State[];
  alpha: FSMAlpha[];
};

export default function useRuleForm(machineType: MachineType) {
  const blankRule = initRule(machineType);

  return (props: RuleFormProps) => {
    const [currentRule, setCurrentRule] = useState(blankRule);
    const [error, setError] = useState('');
    const resetValues = () => {
      setCurrentRule(blankRule);
      setError('');
    };
    const addRule = () => {
      const msg = validateRule(
        currentRule,
        props.rules,
        props.states,
        props.alpha,
      );
      if (!msg) {
        // If we have pda then we want EMP to be a empty array
        if (isPdaRule(currentRule)) {
          currentRule.popped = currentRule.popped.filter((v) => v !== 'ε');
          currentRule.pushed = currentRule.pushed.filter((v) => v !== 'ε');
        }
        props.setRules([...props.rules, currentRule]);
        resetValues();
        props.toggle();
        props.toggleSnack('Rule successfully added');
      } else {
        setError(msg);
      }
    };

    const removeRule = () => {
      const msg = validateRule(
        currentRule,
        props.rules,
        props.states,
        props.alpha,
        false,
      );
      if (msg) {
        setError(msg);
      } else if (props.rules.find((r) => isFSMRuleEqual(r, currentRule))) {
        if (isPdaRule(currentRule)) {
          currentRule.popped = currentRule.popped.filter((v) => v !== 'ε');
          currentRule.pushed = currentRule.pushed.filter((v) => v !== 'ε');
        }
        props.setRules(
          props.rules.filter((r) => !isFSMRuleEqual(r, currentRule)),
        );
        props.toggle();
        props.toggleSnack('Rule was removed');
        resetValues();
      } else {
        setError('Rule does not exist');
      }
    };

    return (
      <GenericForm
        title={'Add/Remove Rule'}
        isOpen={props.isOpen}
        onClose={() => {
          setError('');
          props.toggle();
        }}
        onSubmit={() => addRule()}
        submitText="Add Rule"
        closeText="Remove Rule"
        onCancel={removeRule}
      >
        <DialogContent>
          <DialogContentText>
            Please enter the name of the state that you wish to Add/Remove.
          </DialogContentText>
          <Stack spacing={1}>
            <Stack spacing={1} direction="row">
              {Object.keys(currentRule).map((k: keyof FSMRule) => {
                const parseFunc = (v: string) => {
                  if (isTmType(props.machineType)) {
                    if (v === 'R' || v === 'L' || v === '@' || v === '_') {
                      return parseValueAsString;
                    } else if (
                      (k as keyof TmMttmRule) === 'startTape' ||
                      (k as keyof TmMttmRule) === 'endTape'
                    ) {
                      return parseValueAsArray;
                    } else {
                      return parseValueAsString;
                    }
                  } else {
                    if (Array.isArray(currentRule[k])) {
                      return parseValueAsArray;
                    } else {
                      return parseValueAsString;
                    }
                  }
                };
                return (
                  <RuleInput
                    key={k}
                    label={k}
                    value={unParseValue(currentRule[k])}
                    hasError={!!error}
                    onChange={(v) => {
                      const value = parseData(v, props.machineType);
                      setCurrentRule({
                        ...currentRule,
                        [k]: parseFunc(value)(value),
                      });
                    }}
                  />
                );
              })}
            </Stack>
            <FormControl variant="standard" error={!!error}>
              {error && (
                <FormHelperText
                  id="component-error-text"
                  style={{ textAlign: 'center', paddingBottom: '5px' }}
                >
                  {error}
                </FormHelperText>
              )}
            </FormControl>
          </Stack>
        </DialogContent>
      </GenericForm>
    );
  };
}
