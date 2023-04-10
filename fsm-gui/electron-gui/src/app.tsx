import React, { useState } from 'react';
import ReactDOM from 'react-dom/client';
import { Divider, Grid, Box, StyledEngineProvider } from '@mui/material';
import { State, FSMRule } from './types/machine';
import ControlView from './components/controlView/view';
import MachineEditor from './components/machineEditor/machineEditor';

const TMP_RULES = [
  { start: "A", input: "a", end: "B" },
  { start: "B", input: "b", end: "C" },
  { start: "C", input: "a", end: "A" },
]

const TMP_STATES = [{name: "A", type: "start"}, {name: "B", type: "final"}, {name: "C", type: "normal"}] as State[]


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
    <Box sx={{ flexGrow: 1 }}>
      <Grid container direction="row" spacing={1}>
        <Grid item xs={12}>
          <p>Input</p>
        </Grid>
        <Grid item xs={12}>
          <Grid container direction="row" justifyContent="center" alignItems="center" spacing={2}>
            <Grid item xs={8} justifyContent="center" display="flex">
              <ControlView states={states} currentRule={rules[0]} />
            </Grid>
            <Divider orientation="vertical" flexItem />
            <Grid item xs>
              <MachineEditor addState={addState} removeState={removeState} />
            </Grid>
          </Grid>
        </Grid>
        <Grid item xs={12}>
          <p>Rules</p>
        </Grid>
      </Grid>
    </Box>
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
