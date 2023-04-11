import React, { useState } from 'react';
import ReactDOM from 'react-dom/client';
import { Experimental_CssVarsProvider as CssVarsProvider } from '@mui/material/styles';
import { Paper, Grid, StyledEngineProvider } from '@mui/material';
import { State, FSMRule } from './types/machine';
import ControlView from './components/controlView/view';
import MachineEditorComponent from './components/machineEditor/MachineEditorComponent';
import RuleComponent from './components/ruleView/rulesComponent';

const TMP_RULES = [
  { start: 'A', input: 'a', end: 'B' },
  { start: 'B', input: 'b', end: 'C' },
  { start: 'C', input: 'a', end: 'A' },

  { start: 'A', input: 'a', end: 'B' },
  { start: 'A', input: 'a', end: 'B' },
  { start: 'A', input: 'a', end: 'B' },
  { start: 'A', input: 'a', end: 'B' },
  { start: 'A', input: 'a', end: 'B' },
  { start: 'A', input: 'a', end: 'B' },
  { start: 'A', input: 'a', end: 'B' },
  { start: 'A', input: 'a', end: 'B' },
  { start: 'A', input: 'a', end: 'B' },
  { start: 'B', input: 'b', end: 'C' },
  { start: 'C', input: 'a', end: 'A' },
  { start: 'B', input: 'b', end: 'C' },
  { start: 'C', input: 'a', end: 'A' },
  { start: 'B', input: 'b', end: 'C' },
  { start: 'C', input: 'a', end: 'A' },
  { start: 'B', input: 'b', end: 'C' },
  { start: 'C', input: 'a', end: 'A' },
  { start: 'B', input: 'b', end: 'C' },
  { start: 'C', input: 'a', end: 'A' },
  { start: 'B', input: 'b', end: 'C' },
  { start: 'C', input: 'a', end: 'A' },
  { start: 'B', input: 'b', end: 'C' },
  { start: 'C', input: 'a', end: 'A' },
  { start: 'B', input: 'b', end: 'C' },
  { start: 'C', input: 'a', end: 'A' },
  { start: 'B', input: 'b', end: 'C' },
  { start: 'C', input: 'a', end: 'A' },
];

const TMP_STATES = [
  { name: 'A', type: 'start' },
  { name: 'B', type: 'final' },
  { name: 'C', type: 'normal' },
] as State[];

const App = () => {
  const [states, setStates] = useState<State[]>(TMP_STATES);
  const [rules, setRules] = useState<FSMRule[]>(TMP_RULES);
  const addState = (state: State) => {
    setStates(states.concat([state]));
  };
  const removeState = (incomming: State) => {
    const newStates = states.filter((s) => s.name !== incomming.name);
    setStates(newStates);
  };
  return (
    <CssVarsProvider>
      <Paper sx={{ flexGrow: 1, margin: 'auto' }}>
        <Grid container direction="row" rowSpacing={2}>
          <Grid item xs={12}>
            <p>Input</p>
          </Grid>
          <Grid
            item
            xs={12}
            style={{ border: '1px solid var(--color-border-grey)' }}
          >
            <Grid container direction="row" spacing={1}>
              <Grid item xs={11} justifyContent="center" display="flex">
                <ControlView states={states} currentRule={rules[0]} />
              </Grid>
              <Grid item xs={1} justifyContent="center" display="flex">
                <MachineEditorComponent
                  states={states}
                  addState={addState}
                  removeState={removeState}
                />
              </Grid>
            </Grid>
          </Grid>
          <Grid item xs={12}>
            <RuleComponent
              rules={rules}
              currentRule={{ start: 'A', input: 'a', end: 'B' }}
            />
          </Grid>
        </Grid>
      </Paper>
    </CssVarsProvider>
  );
};

const root = ReactDOM.createRoot(
  document.getElementById('root') as HTMLElement,
);

root.render(
  <React.StrictMode>
    <StyledEngineProvider>
      <App />
    </StyledEngineProvider>
  </React.StrictMode>,
);
