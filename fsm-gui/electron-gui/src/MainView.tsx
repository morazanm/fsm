import { useState, useEffect } from 'react';
import {
  Paper,
  Grid,
  Button,
  Dialog,
  DialogTitle,
  DialogContent,
  DialogContentText,
  DialogActions,
} from '@mui/material';
import {
  State,
  FSMRule,
  FSMAlpha,
  FSMTransition,
  isStartTransition,
  isEndTransition,
  MachineType,
  FSMInterfacePayload,
  isFSMRuleEqual,
} from './types/machine';
import { alpha, useTheme } from '@mui/material/styles';
import ControlView from './components/controlView/view';
import RightEditor from './components/rightEditor/MachineEditorComponent';
import LeftEditor from './components/leftEditor/LeftEditor';
import RuleComponent from './components/ruleDisplay/rulesComponent';
import InputComponent from './components/inputEditor/InputComponent';
import {
  BuildMachineResponse,
  PrebuiltMachineResponse,
} from './socket/responseTypes';
import {
  RacketInterface,
  Instruction,
  SocketResponse,
} from './socket/racketInterface';

// dummy object to symbolize a machine that has not been set to
// fsm-core for verification
const EMPTY_TRANSITIONS = {
  transitions: [] as FSMTransition[],
  index: -1,
  inputIndex: -1,
};

const TMP_RULES = [
  { start: 'S', input: 'a', end: 'F' },
  { start: 'F', input: 'a', end: 'F' },
  { start: 'S', input: 'b', end: 'A' },
  { start: 'A', input: 'a', end: 'F' },
  { start: 'A', input: 'b', end: 'A' },
  { start: 'F', input: 'b', end: 'D' },
  { start: 'D', input: 'a', end: 'D' },
  { start: 'D', input: 'b', end: 'D' },
];

const TMP_STATES = [
  { name: 'S', type: 'start' },
  { name: 'A', type: 'normal' },
  { name: 'F', type: 'final' },
  { name: 'D', type: 'normal' },
] as State[];

const TMP_ALPHA = ['a', 'b'];

const TMP_INPUT = ['a', 'a', 'a', 'a'];

type MachineTransitions = {
  transitions: FSMTransition[];
  index: number;
  inputIndex: number;
};

type BasicDialogProps = {
  onClose: () => void;
  open: boolean;
  title: string;
  body: string;
  bodyStyle?: object;
};

// Popup for beginning and end of machine transitions
const BasicDialog = (props: BasicDialogProps) => {
  return (
    <Dialog
      open={props.open}
      onClose={props.onClose}
      aria-labelledby="alert-dialog-title"
      aria-describedby="alert-dialog-description"
    >
      <DialogTitle id="alert-dialog-title">{props.title}</DialogTitle>
      <DialogContent>
        <DialogContentText
          id="alert-dialog-description"
          style={props.bodyStyle}
        >
          {props.body}
        </DialogContentText>
      </DialogContent>
      <DialogActions>
        <Button onClick={props.onClose}>Close</Button>
      </DialogActions>
    </Dialog>
  );
};

type MachineState = {
  states: State[];
  rules: FSMRule[];
  alphabet: FSMAlpha[];
  input: FSMAlpha[];
  transitions: MachineTransitions;
  type: MachineType;
};

type MainViewProps = {
  toggleTheme: () => void;
  racketBridge: RacketInterface;
};

const MainView = (props: MainViewProps) => {
  const theme = useTheme();
  const machineType = 'dfa';
  const [openDialog, setOpenDialog] = useState<'start' | 'end' | null>(null);
  const [machineState, setMachineState] = useState<MachineState>({
    states: [],
    rules: [],
    alphabet: [],
    input: [],
    transitions: EMPTY_TRANSITIONS,
    type: 'dfa',
  } as MachineState);

  const resetMachineAndSet = (obj: Partial<MachineState>) =>
    setMachineState({
      ...machineState,
      ...obj,
      transitions: EMPTY_TRANSITIONS,
    });

  const setGuiStates = (states: State[]) => {
    resetMachineAndSet({ states: states });
  };
  const setGuiRules = (rules: FSMRule[]) => {
    resetMachineAndSet({ rules: rules });
  };
  const updateInput = (input: FSMAlpha[]) => {
    resetMachineAndSet({ input: input });
  };
  const addAlpha = (incoming: FSMAlpha) => {
    resetMachineAndSet({ alphabet: [...machineState.alphabet, incoming] });
  };
  const removeAlpha = (incoming: FSMAlpha[]) => {
    resetMachineAndSet({
      alphabet: machineState.alphabet.filter((a) => !incoming.includes(a)),
    });
  };

  useEffect(() => {
    // If we have a connection then listen for messages
    // TODO: We can probably abstract this out with a callback
    if (props.racketBridge.client) {
      props.racketBridge.client.on('data', (data: JSON) => {
        const result: SocketResponse<object> = JSON.parse(data.toString());
        if (result.error) {
          //TODO: Handle error case by displaying message in the GUI
        } else if (result.responseType === Instruction.BUILD) {
          const response = result as SocketResponse<BuildMachineResponse>;
          // See if fsm-core added any states, if so then add them
          const new_states = machineState.states.concat(
            response.data.states.filter(
              (s) => !machineState.states.find((st) => st.name === s.name),
            ),
          );
          const new_rules = machineState.rules.concat(
            response.data.rules.filter(
              (r) => !machineState.rules.find((mr) => isFSMRuleEqual(r, mr)),
            ),
          );
          setMachineState({
            ...machineState,
            states: new_states,
            rules: new_rules,
            transitions: {
              transitions: response.data.transitions,
              index: 0,
              inputIndex: -1,
            },
          });
        } else if (result.responseType === Instruction.PREBUILT) {
          const response = result as SocketResponse<PrebuiltMachineResponse>;

          resetMachineAndSet({
            ...machineState,
            states: response.data.states,
            alphabet: response.data.alpha,
            rules: response.data.rules,
            type: response.data.type,
          });
        }
      });
      props.racketBridge.client.on('end', () => {
        //TODO: render something on the screen to indicate that the
        //the server was shut down
        console.log('disconnected from server (on end)');
      });
    }
  }, []);

  const run = () => {
    props.racketBridge.sendToRacket(
      machineState as FSMInterfacePayload,
      Instruction.BUILD,
    );
  };

  // Handles visualizing the next transition
  const goNext = () => {
    if (
      machineState.transitions.index <
      machineState.transitions.transitions.length - 1
    ) {
      setMachineState({
        ...machineState,
        transitions: {
          transitions: machineState.transitions.transitions,
          index: machineState.transitions.index + 1,
          inputIndex: machineState.transitions.inputIndex + 1,
        },
      });
    } else {
      setOpenDialog('end');
    }
  };

  // // Handles visualizing the previous transition
  const goPrev = () => {
    if (machineState.transitions.index > 0) {
      setMachineState({
        ...machineState,
        transitions: {
          transitions: machineState.transitions.transitions,
          index: machineState.transitions.index - 1,
          inputIndex: machineState.transitions.inputIndex - 1,
        },
      });
    } else {
      setOpenDialog('start');
    }
  };

  const getCurrentTransition = (): FSMTransition | undefined => {
    if (
      machineState.transitions.index !== -1 &&
      machineState.transitions.transitions.length !== 0
    ) {
      return machineState.transitions.transitions[
        machineState.transitions.index
      ];
    }
    return undefined;
  };
  const currentTransition = getCurrentTransition();
  return (
    <Paper
      sx={{
        flexGrow: 1,
        margin: 'auto',
        height: '100vh',
        display: 'flex',
        borderRadius: 0,
        bgcolor: theme.palette.background.paper,
        color: theme.palette.text.primary,
      }}
    >
      <Grid container direction="row" rowSpacing={1}>
        <Grid item xs={12}>
          <InputComponent
            inputIndex={machineState.transitions.inputIndex}
            input={machineState.input}
            runMachine={run}
            goNext={goNext}
            goPrev={goPrev}
          />
        </Grid>
        <Grid
          item
          sx={{
            borderRight: `1px solid ${theme.palette.divider}`,
            borderTop: `1px solid ${theme.palette.divider}`,
            borderBottom: `1px solid ${theme.palette.divider}`,
          }}
          xs={12}
        >
          <Grid container direction="row" spacing={1} height="100%">
            <Grid
              item
              height="101.2%"
              xs={1}
              justifyContent="center"
              display="flex"
              sx={{ borderRight: `1px solid ${theme.palette.divider}` }}
            >
              <LeftEditor
                alpha={machineState.alphabet}
                addAlpha={addAlpha}
                removeAlpha={removeAlpha}
              />
            </Grid>
            <Grid
              item
              xs={10}
              justifyContent="center"
              alignItems="center"
              display="flex"
            >
              <ControlView
                states={machineState.states}
                currentTransition={currentTransition}
              />
            </Grid>
            <Grid item xs={1} justifyContent="end" display="flex">
              <RightEditor
                toggleTheme={props.toggleTheme}
                machineType={machineType}
                states={machineState.states}
                setStates={setGuiStates}
                input={machineState.input}
                setInput={updateInput}
                alpha={machineState.alphabet}
                rules={machineState.rules}
                setRules={setGuiRules}
              />
            </Grid>
          </Grid>
        </Grid>
        <Grid
          item
          xs={12}
          display="flex"
          justifyContent="center"
          style={{ paddingTop: '0px' }}
        >
          <RuleComponent
            rules={machineState.rules}
            currentTransition={currentTransition}
          />
        </Grid>
      </Grid>
      {openDialog === 'start' && isStartTransition(currentTransition) && (
        <BasicDialog
          open
          onClose={() => setOpenDialog(null)}
          title="Beginning of Machine"
          body="You have reached the beginning of the machines transitions"
        />
      )}
      {openDialog === 'end' && isEndTransition(currentTransition) && (
        <BasicDialog
          open
          onClose={() => setOpenDialog(null)}
          title="End of Machine"
          body={`The input was ${currentTransition.action}ed.`}
          bodyStyle={{
            color:
              currentTransition.action === 'accept'
                ? theme.palette.success.main
                : theme.palette.error.main,
          }}
        />
      )}
    </Paper>
  );
};

export default MainView;
