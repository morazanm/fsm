import React from 'react';
import ReactDOM from 'react-dom/client';
import AddRemoveForm from './components/AddRemoveForm';
import AddRemoveStateForm from './components/AddRemoveStateForm';
import { useAddRemoveRuleForm } from './components/AddRemoveRuleForm';
import { Divider, Stack } from '@mui/material';
import { FSMRule } from './types/machine';

const App = () => {
  const AddRemoveRuleForm = useAddRemoveRuleForm('dfa');
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
        <AddRemoveRuleForm
          addRule={() => true}
          removeRule={() => true}
          rules={[] as FSMRule[]}
          alphabet={[] as string[]}
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
