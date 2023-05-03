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
  buildFSMInterfacePayload,
  FSMTransition,
  isStartTransition,
  isEndTransition,
} from './types/machine';
import { useTheme } from '@mui/material/styles';
import ControlView from './components/controlView/view';
import RightEditor from './components/rightEditor/MachineEditorComponent';
import LeftEditor from './components/leftEditor/LeftEditor';
import RuleComponent from './components/ruleDisplay/rulesComponent';
import InputComponent from './components/inputEditor/InputComponent';
import { BuildMachineResponse } from './socket/responseTypes';
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
];

const TMP_STATES = [
  { name: 'S', type: 'start' },
  { name: 'A', type: 'normal' },
  { name: 'F', type: 'final' },
] as State[];

const TMP_ALPHA = ['a', 'b'];

const TMP_INPUT = ['a', 'a', 'a', 'a'];

type MainViewProps = {
  toggleTheme: () => void;
  racketBridge: RacketInterface;
};

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

const MainView = (props: MainViewProps) => {
  const theme = useTheme();
  const machineType = 'dfa';
  const [openDialog, setOpenDialog] = useState<'start' | 'end' | null>(null);
  const [states, setStates] = useState<State[]>(TMP_STATES);
  const [rules, setRules] = useState<FSMRule[]>(TMP_RULES);
  const [alphabet, setAlphabet] = useState<FSMAlpha[]>(TMP_ALPHA);
  const [input, setInput] = useState<FSMAlpha[]>(TMP_INPUT);
  const [machineTransitions, setMachineTransitions] =
    useState<MachineTransitions>(EMPTY_TRANSITIONS);

  const resetMachine = () => setMachineTransitions(EMPTY_TRANSITIONS);

  const setGuiStates = (states: State[]) => {
    resetMachine();
    setStates(states);
  };
  const setGuiRules = (rules: FSMRule[]) => {
    resetMachine();
    setRules(rules);
  };
  const updateInput = (input: FSMAlpha[]) => {
    resetMachine();
    setInput(input);
  };
  const addAlpha = (incoming: FSMAlpha) => {
    resetMachine();
    setAlphabet([...alphabet, incoming]);
  };
  const removeAlpha = (incoming: FSMAlpha[]) => {
    resetMachine();
    setAlphabet(alphabet.filter((a) => !incoming.includes(a)));
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
          const tmp = result as SocketResponse<BuildMachineResponse>;
          setMachineTransitions({
            transitions: tmp.data.transitions,
            index: 0,
            inputIndex: -1,
          });
          const new_states = tmp.data.states.filter(
            (s) => !states.find((st) => st.name === s),
          );
          if (new_states) {
            setStates(
              states.concat(
                new_states.map((s) => ({ name: s, type: 'normal' } as State)),
              ),
            );
          }
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
    const machine = buildFSMInterfacePayload(
      states,
      alphabet,
      rules,
      machineType,
      input,
    );
    props.racketBridge.sendToRacket(machine, Instruction.BUILD);
  };

  // Handles visualizing the next transition
  const goNext = () => {
    if (machineTransitions.index < machineTransitions.transitions.length - 1) {
      setMachineTransitions({
        transitions: machineTransitions.transitions,
        index: machineTransitions.index + 1,
        inputIndex: machineTransitions.inputIndex + 1,
      });
    } else {
      setOpenDialog('end');
    }
  };

  // // Handles visualizing the previous transition
  const goPrev = () => {
    if (machineTransitions.index > 0) {
      setMachineTransitions({
        transitions: machineTransitions.transitions,
        index: machineTransitions.index - 1,
        inputIndex: machineTransitions.inputIndex - 1,
      });
    } else {
      setOpenDialog('start');
    }
  };

  const getCurrentTransition = (): FSMTransition | undefined => {
    if (
      machineTransitions.index !== -1 &&
      machineTransitions.transitions.length !== 0
    ) {
      return machineTransitions.transitions[machineTransitions.index];
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
            inputIndex={machineTransitions.inputIndex}
            input={input}
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
                alpha={alphabet}
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
                states={states}
                currentTransition={currentTransition}
              />
            </Grid>
            <Grid item xs={1} justifyContent="end" display="flex">
              <RightEditor
                toggleTheme={props.toggleTheme}
                machineType={machineType}
                states={states}
                setStates={setGuiStates}
                input={input}
                setInput={updateInput}
                alpha={alphabet}
                rules={rules}
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
          <RuleComponent rules={rules} currentTransition={currentTransition} />
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
