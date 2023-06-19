import {
  Alert,
  Button,
  Dialog,
  DialogActions,
  DialogContent,
  DialogContentText,
  DialogTitle,
  FormControl,
  Grid,
  IconButton,
  InputLabel,
  List,
  ListItem,
  ListItemText,
  MenuItem,
  Select,
  SelectChangeEvent,
  Snackbar,
  Stack,
  TextField,
  Typography,
  useTheme,
} from '@mui/material';
import { Prism as SyntaxHighlighter } from 'react-syntax-highlighter';
import { Close as CloseIcon } from '@mui/icons-material';
import {
  StateType,
  State,
  FSMRule,
  MachineType,
  isTmType,
  FSMTransition,
  isNormalTransition,
  isStartTransition,
  isEndTransition,
  FSMAlpha,
  isPdaTransition,
  ruleToString,
} from '../../types/machine';
import { useState } from 'react';
import { validateState } from '../../components/rightEditor/forms/StateForm';
import { MachineState } from '../../view/MainView';

function updateRules(
  oldName: string,
  newName: string,
  rules: FSMRule[],
): FSMRule[] {
  return rules.map((r) => {
    let newStart = r.start === oldName ? newName : r.start;
    let newEnd = r.end === oldName ? newName : r.end;
    return { ...r, start: newStart, end: newEnd };
  });
}

type InvariantDetailsProps = {
  machineType: MachineType;
  currentTransition: FSMTransition | undefined;
  consumedInput: FSMAlpha[] | undefined;
};

const InvariantDetails = (props: InvariantDetailsProps) => {
  const theme = useTheme();
  const formatInvariant = (status: boolean | null): string => {
    if (status === null) return 'None';
    return status ? 'Passing' : 'Failing';
  };

  const invariantColor = (status: boolean | null): string => {
    if (status === null) return theme.palette.secondary.main;
    return status ? theme.palette.success.main : theme.palette.error.main;
  };

  const pdaStack =
    props.currentTransition && isPdaTransition(props.currentTransition)
      ? props.currentTransition.stack
      : [];

  return (
    <Grid
      alignItems="center"
      container
      spacing={2}
      wrap="nowrap"
      sx={{ overflow: 'auto' }}
      style={{ marginTop: '0px' }}
    >
      <Grid item xs={7}>
        <Stack>
          <div
            style={{
              width: '100%',
              height: '100%',
              display: 'inline-flex',
            }}
          >
            <p
              style={{
                paddingRight: '2px',
              }}
            >
              {isTmType(props.machineType) ? 'Tape: ' : 'Consumed Input:'}
            </p>
            <p>
              [
              <span
                style={{
                  overflow: 'auto',
                  whiteSpace: 'nowrap',
                  color: theme.palette.primary.main,
                }}
              >
                {props.consumedInput.join(', ') ?? ' '}
              </span>
              ]
            </p>
          </div>
          {props.currentTransition && props.machineType === 'pda' && (
            <div
              style={{
                width: '100%',
                height: '100%',
                display: 'inline-flex',
              }}
            >
              <p
                style={{
                  paddingRight: '2px',
                }}
              >
                Stack:
              </p>
              <p>
                [
                <span
                  style={{
                    overflow: 'auto',
                    whiteSpace: 'nowrap',
                    color: theme.palette.primary.main,
                  }}
                >
                  {pdaStack.join(', ') ?? ' '}
                </span>
                ]
              </p>
            </div>
          )}
        </Stack>
      </Grid>
      <Grid item xs={5}>
        <Typography>
          Status:{' '}
          <span
            style={{
              color: invariantColor(props.currentTransition.invPass),
            }}
          >
            {formatInvariant(props.currentTransition.invPass)}
          </span>
        </Typography>
      </Grid>
    </Grid>
  );
};

type StateModalProps = {
  open: boolean;
  onClose: () => void;
  onSubmit: (msg: string) => void;
  state: State;
  states: State[];
  rules: FSMRule[];
  machineType: MachineType;
  resetMachineAndSet: (machine: Partial<MachineState>) => void;
  currentTransition: FSMTransition | undefined;
  consumedInput: FSMAlpha[] | undefined;
};

export const StateModal = (props: StateModalProps) => {
  const [type, setType] = useState<StateType>(props.state.type);
  const [stateName, setStateName] = useState(props.state.name);
  const [stateNameHelperText, setStateNameHelperText] = useState('');

  const showUpdateButton = () => {
    return type !== props.state.type || stateName !== props.state.name;
  };
  const formValidation = () => {
    if (stateName !== props.state.name) {
      const isValid = validateState(stateName, props.states);
      setStateNameHelperText(isValid);
      return isValid === '';
    }
    return true;
  };

  const showInvDetails = (t: FSMTransition | undefined): boolean => {
    if (t === undefined) return false;
    if (isNormalTransition(t)) {
      return t.rule.end === props.state.name;
    } else if (isStartTransition(t)) {
      return t.start === props.state.name;
    } else if (isEndTransition(t)) {
      return t.end === props.state.name;
    }
    return false;
  };

  const filterRules = props.rules.filter(
    (r) => r.start === props.state.name || r.end === props.state.name,
  );

  const theme = useTheme();
  return (
    <Dialog
      open={props.open}
      fullWidth
      maxWidth="md"
      onClose={props.onClose}
      aria-describedby="alert-dialog-slide-description"
    >
      <DialogTitle>
        {`State View`}
        <IconButton
          aria-label="close"
          onClick={props.onClose}
          sx={{
            position: 'absolute',
            right: 8,
            top: 8,
            color: theme.palette.grey[500],
          }}
        >
          <CloseIcon />
        </IconButton>
      </DialogTitle>
      <DialogContent>
        <div>
          <FormControl sx={{ m: 1 }}>
            <TextField
              error={stateNameHelperText !== ''}
              id="outlined-required"
              label="State Name"
              defaultValue={props.state.name}
              helperText={stateNameHelperText}
              onChange={(e) => setStateName(e.target.value)}
            />
          </FormControl>
          <FormControl sx={{ m: 1 }}>
            <InputLabel id="demo-simple-select-label">Type</InputLabel>
            <Select
              labelId="demo-simple-select-label"
              id="demo-simple-select"
              value={type}
              label="Type"
              onChange={(event: SelectChangeEvent<StateType>) =>
                setType(event.target.value as StateType)
              }
            >
              <MenuItem value={'normal'}>Normal</MenuItem>
              <MenuItem value={'start'}>Start</MenuItem>
              <MenuItem value={'final'}>Final</MenuItem>
              <MenuItem value={'startFinal'}>StartFinal</MenuItem>
              {isTmType(props.machineType) && (
                <MenuItem value={'accept'}>Accept</MenuItem>
              )}
            </Select>
          </FormControl>
          <hr />
          <Stack spacing={2} sx={{ paddingTop: 2 }}>
            <Typography variant="h6">Invariant:</Typography>
            {showInvDetails(props.currentTransition) && (
              <InvariantDetails
                consumedInput={props.consumedInput}
                currentTransition={props.currentTransition}
                machineType={props.machineType}
              />
            )}
            {props.state.invFunc ? (
              <SyntaxHighlighter language="racket">
                {props.state.invFunc}
              </SyntaxHighlighter>
            ) : (
              <Typography>
                There does not appear to be an invariant function associated
                with this state. Please build a machine with an invariant
                associated with this state to view the invariant here.
              </Typography>
            )}
          </Stack>
          <hr />
          <Stack spacing={2} sx={{ paddingTop: 2 }}>
            <Typography variant="h6">Rules:</Typography>
            <List dense={true} style={{ marginTop: '0px' }}>
              {filterRules.map((r) => (
                <ListItem>
                  <ListItemText primary={ruleToString(r)} />
                </ListItem>
              ))}
            </List>
          </Stack>
        </div>
      </DialogContent>
      <DialogActions>
        <Button onClick={props.onClose}>Close</Button>
        <Button
          disabled={!showUpdateButton()}
          onClick={() => {
            const isValid = formValidation();
            if (isValid) {
              const rules =
                stateName !== props.state.name
                  ? updateRules(props.state.name, stateName, props.rules)
                  : props.rules;
              const states = props.states.map((s) =>
                s.name === props.state.name
                  ? { ...s, name: stateName, type: type }
                  : s,
              );
              props.resetMachineAndSet({
                states: states,
                rules: rules,
              });
              props.onSubmit(`State ${stateName} was updated.`);
              props.onClose();
            }
          }}
        >
          Update
        </Button>
      </DialogActions>
    </Dialog>
  );
};
