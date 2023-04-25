import { useState } from 'react';
import TextField from '@mui/material/TextField';
import Button from '@mui/material/Button';
import DialogContent from '@mui/material/DialogContent';
import DialogContentText from '@mui/material/DialogContentText';
import GenericForm from './GenericForm';
import { MachineType, isTmType, FSMAlpha } from '../../../types/machine';

type InputFormProps = {
  isOpen: boolean;
  machineType: MachineType;
  toggle: () => void;
  input: FSMAlpha[];
  setInput: (incomming: FSMAlpha[]) => void;
  alpha: FSMAlpha[];
};

const validateInput = (incomming: string[], alpha: FSMAlpha[]) => {
  const badAlpha = incomming.filter((v) => !alpha.includes(v));
  return badAlpha.length > 0
    ? `The following are not in the list of alpha: ${badAlpha}`
    : '';
};

export default function InputForm(props: InputFormProps) {
  const title = isTmType(props.machineType) ? 'Tape Input' : 'Machine Input';
  const [data, setData] = useState('');
  const [error, setError] = useState('');

  const onSubmit = (incomming: string) => {
    const tmp = incomming.trim().split(/\s+/);
    const msg = validateInput(tmp, props.alpha);
    if (msg != '') {
      setError(msg);
    } else {
      props.setInput(props.input.concat(tmp));
      setError('');
      props.toggle();
    }
  };
  return (
    <GenericForm
      title={title}
      isOpen={props.isOpen}
      onClose={() => {
        setError('');
        props.toggle();
      }}
      onSubmit={() => onSubmit(data)}
      submitText="Append Input"
    >
      <DialogContent>
        <DialogContentText>
          Please enter the machine input below. You can use spaces to seperate
          values.
        </DialogContentText>
        <TextField
          autoFocus
          error={error !== ''}
          margin="dense"
          id="name"
          label="Input"
          type="normal"
          fullWidth
          variant="standard"
          helperText={error}
          onChange={(e) => setData(e.target.value)}
        />
        <Button
          variant="outlined"
          onClick={() => {
            props.setInput([]);
          }}
          color="error"
          sx={{ marginTop: 3 }}
        >
          Clear Current
        </Button>
      </DialogContent>
    </GenericForm>
  );
}
