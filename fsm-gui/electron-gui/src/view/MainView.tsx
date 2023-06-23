import { useState, useEffect, useRef } from 'react';
import { ipcRenderer } from 'electron';
import { Prism as SyntaxHighlighter } from 'react-syntax-highlighter';
import {
  Paper,
  Grid,
  Box,
  Backdrop,
  CircularProgress,
  Typography,
} from '@mui/material';
import {
  State,
  FSMRule,
  FSMAlpha,
  FSMTransition,
  isStartTransition,
  isEndTransition,
  MachineType,
  FSMStackAlpha,
  isTmType,
  isTmMttmTransition,
  MachineTransitions,
} from '../types/machine';
import { useTheme } from '@mui/material/styles';
import { BasicDialog, IncomingMachineDialog } from './dialog';
import ControlView from '../components/controlView/view';
import RightEditor from '../components/rightEditor/MachineEditorComponent';
import LeftEditor from '../components/leftEditor/LeftEditor';
import RuleComponent from '../components/ruleDisplay/rulesComponent';
import InputComponent from '../components/inputEditor/InputComponent';
import Stack from '../components/stack/Stack';

import {
  RacketInterface,
  Instruction,
  Connection,
} from '../socket/racketInterface';
import {
  FSMBuildMachineRequest,
  RedrawnGraphvizImageRequest,
} from '../socket/requestTypes';
import { parseDataResponse } from './responseParser';
import { channels } from '../shared/constants';
import { saveMachine } from './saveMachine';

// dummy object to symbolize a machine that has not been sent to
// fsm-core for verification
const EMPTY_TRANSITIONS = {
  transitions: [] as FSMTransition[],
  index: -1,
  inputIndex: -1,
};

//HACK: this should be reworked to hold the type and the JSX component. This
// will remove the need to the extra data fields
type DialogData = {
  openDialog: 'start' | 'end' | 'info' | 'error' | 'incoming-machine' | 'none';
  title?: string;
  message?: string | JSX.Element;
  accept?: () => void;
  reject?: () => void;
  type?: MachineType;
};

export type View = 'control' | 'graphViz';

export type MachineState = {
  states: State[];
  rules: FSMRule[];
  alphabet: FSMAlpha[];
  stackAlpha: FSMStackAlpha[]; // only used for pda's
  input: FSMAlpha[];
  transitions: MachineTransitions;
  type: MachineType;
  nodead: boolean;
  initialTapePosition: number; // only used by tm and tm-lang-rec
  accept: State | null; // only used by tm-lang-rec
  graphVizImage: string | null;
  hotReload: boolean;
};

type MainViewProps = {
  toggleTheme: () => void;
  racketBridge: RacketInterface;
};

const MainView = (props: MainViewProps) => {
  const theme = useTheme();
  const [openDialog, setOpenDialog] = useState<DialogData>({
    openDialog: 'none',
  });
  const [connected, setConnected] = useState<Connection>({
    connected: props.racketBridge.connected,
    status: 'done',
  });
  const skipRedraw = useRef(true);
  const [waitingForResponse, setWaitingForResponse] = useState(false);
  const [view, setView] = useState<View>('control');
  const [machineState, setMachineState] = useState<MachineState>({
    states: [],
    rules: [],
    alphabet: [],
    stackAlpha: [],
    input: [],
    transitions: EMPTY_TRANSITIONS,
    type: 'dfa',
    nodead: true,
    accept: null,
    initialTapePosition: 0,
    graphVizImage: null,
    hotReload: false,
  } as MachineState);

  const machineStateRef = useRef(machineState);
  const resetMachineAndSet = (obj: Partial<MachineState>) =>
    setMachineState({
      ...machineState,
      ...obj,
      transitions: EMPTY_TRANSITIONS,
    });

  const openInfoDialog = (title: string, message: string | JSX.Element) => {
    setOpenDialog({ openDialog: 'info', title, message });
  };

  const openErrorDialog = (title: string, message: string) => {
    setOpenDialog({ openDialog: 'error', title, message });
  };

  const setGuiView = (newView: View) => {
    if (newView !== view) {
      setView(newView);
    }
  };

  const setTransIndex = (idx: number) => {
    setMachineState({
      ...machineState,
      transitions: {
        transitions: machineState.transitions.transitions,
        inputIndex: idx,
        index: idx,
      },
    });
  };

  const redrawnGraph = () => {
    props.racketBridge.sendToRacket(
      {
        ...machineState,
        currentFilepath: machineState.graphVizImage,
      } as RedrawnGraphvizImageRequest,
      Instruction.REDRAW,
    );
  };

  // We only need to remake the graphViz image when the states or
  // rules change.
  useEffect(() => {
    if (!skipRedraw.current) {
      redrawnGraph();
    }
    skipRedraw.current = false;
  }, [machineState.states, machineState.rules]);

  const toggleDead = () =>
    setMachineState({ ...machineState, nodead: !machineState.nodead });
  const setGuiStates = (states: State[]) => {
    const stateNames = states.map((s) => s.name);
    const new_rules = machineStateRef.current.rules.filter(
      (rule) =>
        stateNames.includes(rule.start) && stateNames.includes(rule.end),
    );
    resetMachineAndSet({ states: states, rules: new_rules });
  };
  const setInitialTapePosition = (position: number) => {
    resetMachineAndSet({
      initialTapePosition: position,
    });
  };
  const setGuiRules = (rules: FSMRule[]) => {
    resetMachineAndSet({ rules: rules });
  };
  const updateInput = (input: FSMAlpha[]) => {
    resetMachineAndSet({ input: input });
  };
  const setAlpha = (alpha: FSMAlpha[]) => {
    resetMachineAndSet({ alphabet: alpha });
  };
  const setStackAlpha = (stackAlpha: FSMStackAlpha[]) => {
    resetMachineAndSet({ stackAlpha: stackAlpha });
  };

  const attemptToReconnect = () => {
    setConnected({ ...connected, status: 'attempting' });
    props.racketBridge.establishConnection().then((res) => {
      setConnected({ connected: res, status: 'done' });
    });
  };

  const pathToGraphvizImage = () => {
    if (currentTransition && currentTransition.filepath) {
      return `file://${currentTransition.filepath}`;
    }
    return machineState.graphVizImage
      ? `file://${machineState.graphVizImage}`
      : '';
  };

  useEffect(() => {
    setConnected({ connected: props.racketBridge.connected, status: 'done' });
  }, [props.racketBridge.connected]);

  useEffect(() => {
    machineStateRef.current = machineState;
  }, [machineState]);

  useEffect(() => {
    // If we have a connection then listen for messages
    if (props.racketBridge.client) {
      props.racketBridge.subscribeListener('data', (response) => {
        setWaitingForResponse(false);
        const { data, instruction } = parseDataResponse(
          response,
          machineStateRef.current,
        );
        if (typeof data === 'string') {
          if (instruction === Instruction.REDRAW) {
            setMachineState({
              ...machineStateRef.current,
              graphVizImage: data,
            });
          } else {
            openErrorDialog('Error Building Machine', data);
          }
        } else {
          if (instruction === Instruction.PREBUILT) {
            const acceptFunction = () => {
              skipRedraw.current = true;
              resetMachineAndSet(data);
              setOpenDialog({ openDialog: 'none' });
            };
            setOpenDialog({
              openDialog: 'incoming-machine',
              accept: acceptFunction,
              type: data.type,
            });
          } else if (instruction === Instruction.BUILD) {
            skipRedraw.current = true;
            setMachineState(data);
            openInfoDialog(
              'Machine Successfully Built',
              'The machine was successfully built. You may now use the next and prev buttons to visualize the machine.',
            );
          }
          // Add files to track for clean up
          const filesToTrack = data.transitions.transitions.map(
            (t) => t.filepath,
          );
          //HACK: vizTool_electron1 needs to stay so the server and stay up and still send
          // the image if a new instance of the GUI is spun up.
          //TODO: Once all files are unique named this will not a problem
          if (data.graphVizImage !== '/var/tmp/vizTool_electron1.svg') {
            filesToTrack.push(data.graphVizImage);
          }
          ipcRenderer.send(channels.TRACK_FILE, filesToTrack);
        }
      });
      props.racketBridge.subscribeListener('end', () => {
        openInfoDialog(
          'Disconnected from FSM',
          <div>
            <Typography component={'span'}>
              The FSM backend was disconnected. You can still save and edit your
              machine, but you will not be able to rebuild it.
              <br />
              To reconnect Please try running in the Racket REPL:
            </Typography>
            <SyntaxHighlighter language="racket">
              (sm-visualize2 ...)
            </SyntaxHighlighter>
          </div>,
        );
      });

      // Handle calls the to the main process
      ipcRenderer.on(channels.SAVE_FILE, (event, filepath) => {
        if (saveMachine(filepath, machineStateRef.current)) {
          openInfoDialog(
            'Save Machine',
            `Successfully saved machine to:\n ${filepath}`,
          );
        } else {
          openErrorDialog(
            'Save Error',
            'Unable to save machine. Check file/directory permissions',
          );
        }
      });

      // Clean up all listeners
      return () => {
        ipcRenderer.removeAllListeners(channels.SAVE_FILE);
      };
    }
  }, []);

  const run = () => {
    props.racketBridge.sendToRacket(
      {
        ...machineState,
        tapeIndex: machineState.initialTapePosition,
      } as FSMBuildMachineRequest,
      Instruction.BUILD,
    );
    setWaitingForResponse(true);
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
      setOpenDialog({ openDialog: 'end' });
    }
  };

  // Handles visualizing the previous transition
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
      setOpenDialog({ openDialog: 'start' });
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

  const determineInputOrTapeIndex = () => {
    if (isTmType(machineState.type)) {
      if (currentTransition && isTmMttmTransition(currentTransition)) {
        return currentTransition.tapeIndex;
      }
      return machineState.initialTapePosition;
      //TODO: Add logic to see if machine was ran, if so use the transitions...
    } else {
      return machineState.transitions.inputIndex;
    }
  };
  // For Tm's we use the transitions if the machine was ran,
  // otherwise use the machineState.input
  const getTapeInput = () => {
    if (currentTransition && isTmMttmTransition(currentTransition)) {
      return currentTransition.tape;
    }
    return machineState.input;
  };

  const getConsumedInput = () => {
    if (currentTransition) {
      return machineState.input.slice(
        0,
        machineState.transitions.inputIndex + 1,
      );
    }
    return undefined;
  };

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
            states={machineState.states}
            inputIndex={determineInputOrTapeIndex()}
            input={getTapeInput()}
            runMachine={run}
            goNext={goNext}
            goPrev={goPrev}
            transitions={machineState.transitions.transitions}
            type={machineState.type}
            isConnected={props.racketBridge.connected}
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
                stackAlpha={machineState.stackAlpha}
                type={machineState.type}
              />
            </Grid>
            {machineState.type === 'pda' && (
              <Grid
                item
                height="101.2%"
                xs={1}
                justifyContent="center"
                display="flex"
                style={{ paddingLeft: '0px' }}
                sx={{ borderRight: `1px solid ${theme.palette.divider}` }}
              >
                <Stack currentTransition={currentTransition} />
              </Grid>
            )}
            <Grid
              item
              xs={machineState.type === 'pda' ? 9 : 10}
              justifyContent="center"
              alignItems="center"
              display="flex"
              sx={{ height: '70vh' }}
            >
              {view === 'control' ? (
                <ControlView
                  states={machineState.states}
                  currentTransition={currentTransition}
                  resetMachineAndSet={resetMachineAndSet}
                  consumedInput={
                    isTmType(machineState.type)
                      ? getTapeInput()
                      : getConsumedInput()
                  }
                  rules={machineState.rules}
                  machineType={machineState.type}
                />
              ) : (
                <Box
                  sx={{
                    justifyContent: 'center',
                    alignItems: 'center',
                    display: 'flex',
                    height: '100%',
                    width: '100%',
                  }}
                >
                  <img
                    src={pathToGraphvizImage()}
                    style={{ width: '85%', maxHeight: '95%' }}
                  />
                </Box>
              )}
            </Grid>
            <Grid item xs={1} justifyContent="end" display="flex">
              <RightEditor
                toggleTheme={props.toggleTheme}
                machineType={machineState.type}
                states={machineState.states}
                setStates={setGuiStates}
                input={machineState.input}
                setInput={updateInput}
                alpha={machineState.alphabet}
                stackAlpha={machineState.stackAlpha}
                setStackAlpha={setStackAlpha}
                rules={machineState.rules}
                setRules={setGuiRules}
                nodead={machineState.nodead}
                toggleDead={toggleDead}
                connection={connected}
                reconnect={attemptToReconnect}
                setAlpha={setAlpha}
                tapePosition={machineState.initialTapePosition}
                setTapePosition={setInitialTapePosition}
                setView={setGuiView}
                currentView={view}
                transitions={machineState.transitions}
                currentTransIndex={machineState.transitions.index}
                setTransIndex={setTransIndex}
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
      {openDialog.openDialog === 'start' &&
        isStartTransition(currentTransition) && (
          <BasicDialog
            open
            onClose={() => setOpenDialog({ openDialog: 'none' })}
            title="Beginning of Machine"
            body="You have reached the beginning of the machines transitions"
          />
        )}
      {openDialog.openDialog === 'end' &&
        isEndTransition(currentTransition) && (
          <BasicDialog
            open
            onClose={() => setOpenDialog({ openDialog: 'none' })}
            title="End of Machine"
            body={`The input was ${
              machineState.type === 'tm-language-recognizer'
                ? 'accept'
                : currentTransition.action
            }ed.`}
            bodyStyle={{
              color:
                currentTransition.action === 'accept' ||
                machineState.type === 'tm-language-recognizer'
                  ? theme.palette.success.main
                  : theme.palette.error.main,
            }}
          />
        )}
      {openDialog.openDialog === 'info' &&
        openDialog.message &&
        openDialog.title && (
          <BasicDialog
            open
            onClose={() => {
              setOpenDialog({ openDialog: 'none' });
            }}
            title={openDialog.title}
            body={openDialog.message}
          />
        )}
      {openDialog.openDialog === 'error' &&
        openDialog.message &&
        openDialog.title && (
          <BasicDialog
            open
            onClose={() => {
              setOpenDialog({ openDialog: 'none' });
            }}
            title={openDialog.title}
            body={openDialog.message}
            bodyStyle={{
              color: theme.palette.error.main,
            }}
          />
        )}
      {openDialog.openDialog === 'incoming-machine' && openDialog.accept && (
        <IncomingMachineDialog
          open
          incomingType={openDialog.type}
          accept={openDialog.accept}
          reject={() => setOpenDialog({ openDialog: 'none' })}
        />
      )}
      {waitingForResponse && (
        <div>
          <Backdrop
            sx={{ color: '#fff', zIndex: (theme) => theme.zIndex.drawer + 1 }}
            open={waitingForResponse}
          >
            <CircularProgress color="inherit" />
          </Backdrop>
        </div>
      )}
    </Paper>
  );
};

export default MainView;
