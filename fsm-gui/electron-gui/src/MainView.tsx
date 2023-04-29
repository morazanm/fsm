import { useState, useEffect } from 'react';
import { Paper, Grid } from '@mui/material';
import {
  State,
  FSMRule,
  FSMAlpha,
  buildFSMInterfacePayload,
} from './types/machine';
import { useTheme } from '@mui/material/styles';
import ControlView from './components/controlView/view';
import RightEditor from './components/rightEditor/MachineEditorComponent';
import LeftEditor from './components/leftEditor/LeftEditor';
import RuleComponent from './components/ruleDisplay/rulesComponent';
import InputComponent from './components/inputEditor/InputComponent';
import { RacketInterface, Instruction } from "./socket/racketInterface";

const TMP_RULES = [
  {start: "S", input: "a", end: "F"},
  {start: "F", input: "a", end: "F"},
  {start: "S", input: "b", end: "A"},
  {start: "A", input: "a", end: "F"},
  {start: "A", input: "b", end: "A"},
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
  racketInterface: RacketInterface;
};

const MainView = (props: MainViewProps) => {
  const theme = useTheme();
  const machineType = 'dfa';
  const [states, setStates] = useState<State[]>(TMP_STATES);
  const [rules, setRules] = useState<FSMRule[]>(TMP_RULES);
  const [alphabet, setAlphabet] = useState<FSMAlpha[]>(TMP_ALPHA);
  const [input, setInput] = useState<FSMAlpha[]>(TMP_INPUT);
  const [inputIndex, setInputIndex] = useState<number | null>(null);

  const setGuiStates = (states: State[]) => setStates(states);
  const setGuiRules = (rules: FSMRule[]) => setRules(rules);
  const addInput = (incoming: FSMAlpha[]) => setInput(input.concat(incoming));
  const clearInput = () => setInput([]);
  const addAlpha = (incoming: FSMAlpha) => setAlphabet([...alphabet, incoming]);
  const removeAlpha = (incoming: FSMAlpha[]) =>
    setAlphabet(alphabet.filter((a) => !incoming.includes(a)));

  useEffect(() => {
    // If we have a connection then listen for messages
    // TODO: We can probs abstract this out with a callback
    if (props.racketInterface.client) {
      props.racketInterface.client.on('data', (data: any) => {
        console.log("recieved:")
        console.log(JSON.parse(data.toString()));
      });
      props.racketInterface.client.on('end', () => {
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
    props.racketInterface.sendToRacket(machine, Instruction.BUILD);
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
            inputIndex={inputIndex}
            input={input}
            addInput={addInput}
            clearInput={clearInput}
            runMachine={run}
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
              <ControlView states={states} currentRule={rules[0]} />
            </Grid>
            <Grid item xs={1} justifyContent="end" display="flex">
              <RightEditor
                toggleTheme={props.toggleTheme}
                machineType={machineType}
                states={states}
                setStates={setGuiStates}
                input={input}
                setInput={(incomming: FSMAlpha[]) => setInput(incomming)}
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
          <RuleComponent
            rules={rules}
            currentRule={{ start: 'A', input: 'a', end: 'B' }}
          />
        </Grid>
      </Grid>
    </Paper>
  );
};

export default MainView;
