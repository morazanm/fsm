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
  DfaNdfaRule,
  isFSMRuleEqual,
} from '../../../types/machine';
import { FormHelperText, Input, InputLabel } from '@mui/material';

function validateRule(
  rule: FSMRule,
  rules: FSMRule[],
  alpha: string[],
): string {
  const fmtMsgs = (msgs: string[]) => msgs.filter((m) => m !== '').join('. ');

  if (Object.values(rule).filter((v) => v === '').length > 0) {
    return 'Please fill in all the rules inputs';
  }
  if (isTmTmLangRecRule(rule)) {
    throw Error('TODO: Validate tm rule');
  } else if (isPdaRule(rule)) {
    throw Error('TODO: Validate pda rule');
  } else if (isDfaNdfaRule(rule)) {
    const dfaNdfaRules = rules as DfaNdfaRule[]; //HACK: we can assume the rules are the same as the machine type so we will just cast
    const alphaMsg = alpha.includes(rule.input)
      ? ''
      : `'${rule.input}' is not in the alphabet`;
    const startMsg = dfaNdfaRules.find((r) => r.start === rule.start)
      ? ''
      : `'${rule.start}' is not in the list of rules`;
    const endMsg = dfaNdfaRules.find((r) => r.end === rule.end)
      ? ''
      : `'${rule.end}' is not in the list of rules`;
    return fmtMsgs([startMsg, alphaMsg, endMsg]);
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
      return { start: '', input: '', startStack: [], end: '', endStack: [] };
    case 'tm':
      return { start: '', startTape: [], end: '', endTape: [] };
    case 'tm-lang-rec':
      return { start: '', startTape: [], end: '', endTape: [] };
  }
}

type RuleInputProps = {
  value: any; //TODO: clean this type up to work with arrays
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
        onChange={(e) => props.onChange(e.target.value.trim())}
      />
    </FormControl>
  );
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
      const msg = validateRule(currentRule, props.rules, props.alpha);
      if (!msg) {
        props.setRules([...props.rules, currentRule]);
        resetValues();
        props.toggle();
        props.toggleSnack('Rule successfully added');
      } else {
        setError(msg);
      }
    };

    const removeRule = () => {
      const msg = validateRule(currentRule, props.rules, props.alpha);
      if (msg) {
        setError(msg);
      } else if (props.rules.find((r) => isFSMRuleEqual(r, currentRule))) {
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
        title={'Add/Remove State'}
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
              {Object.keys(currentRule).map((k: keyof FSMRule) => (
                <RuleInput
                  key={k}
                  label={k}
                  value={currentRule[k]}
                  hasError={!!error}
                  onChange={(v) => setCurrentRule({ ...currentRule, [k]: v })}
                />
              ))}
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
