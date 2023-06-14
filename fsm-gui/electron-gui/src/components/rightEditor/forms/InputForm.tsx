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
  setInput: (incoming: FSMAlpha[]) => void;
  alpha: FSMAlpha[];
};

const isReserved = (s: string): boolean => {
  return s === '@' || s === '_';
};

const validateInput = (incoming: string[], alpha: FSMAlpha[]) => {
  const badAlpha = incoming.filter((v) => !alpha.includes(v) && !isReserved(v));
  return badAlpha.length > 0
    ? `The following are not in the list of alpha: ${badAlpha}`
    : '';
};

const parseData = (data: string) => {
  const tmp = data.split(' ');
  return tmp
    .map((v) => {
      if (v == 'BLANK') {
        return '_';
      } else if (v === 'LM') {
        return '@';
      } else {
        return v;
      }
    })
    .join(' ');
};

export default function InputForm(props: InputFormProps) {
  const title = isTmType(props.machineType) ? 'Tape Input' : 'Machine Input';
  const [data, setData] = useState('');
  const [error, setError] = useState('');

  const onSubmit = (incoming: string) => {
    const tmp = incoming.trim().split(/\s+/);
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
          Please enter the machine input below. You can use spaces to separate
          values.
        </DialogContentText>
        <TextField
          value={data}
          autoFocus
          error={error !== ''}
          margin="dense"
          id="name"
          label="Input"
          type="normal"
          fullWidth
          variant="standard"
          helperText={error}
          onChange={(e) =>
            isTmType(props.machineType)
              ? setData(parseData(e.target.value))
              : setData(e.target.value)
          }
        />
        <Button
          variant="outlined"
          onClick={() => {
            if (isTmType(props.machineType)) {
              props.setInput(['@']);
            } else {
              props.setInput([]);
            }
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
