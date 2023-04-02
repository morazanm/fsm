import React, { useState } from 'react';
import ReactDOM from 'react-dom/client';
import { Divider, Grid, Box, StyledEngineProvider } from '@mui/material';
import { State } from './types/machine';
import ControlView from './components/controlView/view';
import MachineEditor from './components/machineEditor/machineEditor';

const App = () => {
  const [states, setStates] = useState<State[]>([]);
  const addState = (state: State) => {
    setStates(states.concat([state]));
  };

  const removeState = (incomming: State) => {
    const newStates = states.filter((s) => s.name !== incomming.name);
    setStates(newStates);
  };

  return (
    <Box sx={{ flexGrow: 1 }}>
      <Grid container spacing={2}>
        <Grid item xs={8}>
          <ControlView states={states} />
        </Grid>
        <Divider orientation="vertical" flexItem />
        <Grid item xs>
          <MachineEditor addState={addState} removeState={removeState} />
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
