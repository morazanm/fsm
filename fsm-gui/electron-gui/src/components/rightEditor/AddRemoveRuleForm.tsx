import { useState } from 'react';
import { Delete as DeleteIcon, Add as AddIcon } from '@mui/icons-material';
import {
  Button,
  Input,
  FormControl,
  FormHelperText,
  InputLabel,
  Box,
  Stack,
} from '@mui/material';
import {
  FSMRule,
  MachineType,
  DfaNdfaRule,
  isTmTmLangRecRule,
  isPdaRule,
  isDfaNdfaRule,
} from '../../types/machine';

function validateRule(
  rule: FSMRule,
  rules: FSMRule[],
  alpha: string[],
): string | null {
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

type AddRemoveRuleFormProps = {
  addRule: (rule: FSMRule) => void;
  removeRule: (rule: FSMRule) => void;
  rules: FSMRule[];
  alphabet: string[];
};

export const useAddRemoveRuleForm = (machineType: MachineType) => {
  const blankRule = initRule(machineType);
  return (props: AddRemoveRuleFormProps) => {
    const [currentRule, setCurrentRule] = useState(blankRule);
    const [errorMsg, setErrorMsg] = useState(null);
    const resetValues = () => {
      setCurrentRule(blankRule);
      setErrorMsg(null);
    };

    return (
      <Box
        sx={{
          display: 'flex',
          justifyContent: 'center',
          textAlign: 'center',
        }}
      >
        <Stack spacing={1}>
          <Stack spacing={1} direction="row">
            {Object.keys(currentRule).map((k) => (
              <RuleInput
                key={k}
                label={k}
                value={currentRule[k] as any}
                hasError={!!errorMsg}
                onChange={(v) => setCurrentRule({ ...currentRule, [k]: v })}
              />
            ))}
          </Stack>
          <FormControl variant="standard" error={!!errorMsg}>
            {errorMsg && (
              <FormHelperText
                id="component-error-text"
                style={{ textAlign: 'center', paddingBottom: '5px' }}
              >
                {errorMsg}
              </FormHelperText>
            )}
            <Stack
              direction="row"
              spacing={2}
              style={{ justifyContent: 'center' }}
            >
              <Button
                size="small"
                variant="contained"
                color="primary"
                startIcon={<AddIcon />}
                onClick={() => {
                  const msg = validateRule(
                    currentRule,
                    props.rules,
                    props.alphabet,
                  );
                  if (!msg) {
                    props.addRule(currentRule);
                    resetValues();
                  } else {
                    setErrorMsg(msg);
                  }
                }}
              >
                Add
              </Button>
              <Button
                size="small"
                variant="contained"
                color="primary"
                startIcon={<DeleteIcon />}
                onClick={() => {
                  const msg = validateRule(
                    currentRule,
                    props.rules,
                    props.alphabet,
                  );
                  if (msg) {
                    props.removeRule(currentRule);
                    resetValues();
                  } else {
                    setErrorMsg(msg);
                  }
                }}
              >
                Remove
              </Button>
            </Stack>
          </FormControl>
        </Stack>
      </Box>
    );
  };
};
