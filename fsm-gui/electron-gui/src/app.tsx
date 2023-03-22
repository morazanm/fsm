import React from 'react';
import ReactDOM from 'react-dom/client';
import AddRemoveForm from './components/AddRemoveForm';
import AddRemoveStateForm from './components/AddRemoveStateForm';
import { Divider, Stack } from '@mui/material';

const App = () => {
  return (
    <div style={{}}>
      <Stack
        spacing={4}
        divider={<Divider orientation="horizontal" flexItem />}
      >
        <AddRemoveStateForm
          onDelete={(_) => false}
          onSubmit={(_) => false}
          validate={(_) => true}
        />
        <AddRemoveForm
          label="Alpha"
          onDelete={(_) => false}
          onSubmit={(_) => false}
          validate={(_) => true}
        />
      </Stack>
    </div>
  );
};

const root = ReactDOM.createRoot(
  document.getElementById('root') as HTMLElement,
);

root.render(
  <React.StrictMode>
    <App />
  </React.StrictMode>,
);
