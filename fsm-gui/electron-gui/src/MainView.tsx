import { useState } from 'react';
import { Paper, Grid } from '@mui/material';
import { State, FSMRule, FSMAlpha } from './types/machine';
import { useTheme } from '@mui/material/styles';
import ControlView from './components/controlView/view';
import RightEditor from './components/rightEditor/MachineEditorComponent';
import LeftEditor from './components/leftEditor/LeftEditor';
import RuleComponent from './components/ruleDisplay/rulesComponent';
import InputComponent from './components/inputEditor/InputComponent';
const TMP_RULES = [
  { start: 'A', input: 'a', end: 'B' },
  { start: 'B', input: 'b', end: 'C' },
  { start: 'C', input: 'a', end: 'A' },
];

const TMP_STATES = [
  { name: 'A', type: 'start' },
  { name: 'B', type: 'final' },
  { name: 'C', type: 'normal' },
] as State[];

const TMP_ALPHA = ['a', 'b', 'c'];

type MainViewProps = {
  toggleTheme: () => void;
};

const MainView = (props: MainViewProps) => {
  const theme = useTheme();
  const machineType = 'dfa';
  const [states, setStates] = useState<State[]>(TMP_STATES);
  const [rules, setRules] = useState<FSMRule[]>(TMP_RULES);
  const [alphabet, setAlphabet] = useState<FSMAlpha[]>(TMP_ALPHA);
  const [input, setInput] = useState<FSMAlpha[]>(['a', 'b', 'c', 'a']);
  const [inputIndex, setInputIndex] = useState(1);

  const addState = (state: State) => setStates(states.concat([state]));
  const removeState = (incoming: State) =>
    setStates(states.filter((s) => s.name !== incoming.name));
  const addInput = (incoming: FSMAlpha[]) => setInput(input.concat(incoming));
  const clearInput = () => setInput([]);
  const addAlpha = (incoming: FSMAlpha) => setAlphabet([...alphabet, incoming]);
  const removeAlpha = (incoming: FSMAlpha[]) =>
    setAlphabet(alphabet.filter((a) => !incoming.includes(a)));

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
                addState={addState}
                removeState={removeState}
                input={input}
                setInput={(incomming: FSMAlpha[]) => setInput(incomming)}
                alpha={alphabet}
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
