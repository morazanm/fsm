import React from 'react';
import AddRemoveForm from './AddRemoveForm';
import AddRemoveStateForm from './AddRemoveStateForm';
import { useAddRemoveRuleForm } from './AddRemoveRuleForm';
import { Divider, Stack } from '@mui/material';
import { FSMRule, State } from '../../types/machine';

type MachineEditorProps = {
  addState: (state: State) => void;
  removeState: (state: State) => void;
};

const MachineEditor = (props: MachineEditorProps) => {
  const AddRemoveRuleForm = useAddRemoveRuleForm('dfa');
  return (
    <div style={{ paddingRight: '16px' }}>
      <Stack
        spacing={4}
        divider={<Divider orientation="horizontal" flexItem />}
      >
        <AddRemoveStateForm
          onDelete={props.removeState}
          onSubmit={props.addState}
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

export default MachineEditor;
