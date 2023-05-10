import { useState } from 'react';
import TextField from '@mui/material/TextField';
import Radio from '@mui/material/Radio';
import RadioGroup from '@mui/material/RadioGroup';
import DialogContent from '@mui/material/DialogContent';
import DialogContentText from '@mui/material/DialogContentText';
import FormControl from '@mui/material/FormControl';
import FormControlLabel from '@mui/material/FormControlLabel';
import GenericForm from './GenericForm';
import {
  MachineType,
  isTmType,
  State,
  StateName,
  StateType,
} from '../../../types/machine';

type InputFormProps = {
  isOpen: boolean;
  machineType: MachineType;
  toggle: () => void;
  toggleSnack: (msg: string) => void;
  states: State[];
  setStates: (states: State[]) => void;
};

const validateState = (incoming: StateName, states: State[]): string => {
  if (states.find((s) => s.name === incoming)) {
    return `State "${incoming}" already exists`;
  }
  // State: An uppercase letter (e.g., A) or a symbol comprised of an uppercase letter,
  // dash, and number (e.g., A-72431) OR ds.
  const regex = new RegExp(/[A-Z](-\d+)*$/g);
  if (regex.test(incoming) || incoming == 'ds') {
    return '';
  } else {
    return `The state: "${incoming}" is not in proper form`;
  }
};

export default function StateForm(props: InputFormProps) {
  const [state, setState] = useState('');
  const [type, setType] = useState<StateType>('normal');
  const [error, setError] = useState('');

  const resetValues = () => {
    setType('normal');
    setError('');
    setState('');
  };

  const addState = () => {
    const incoming = state.trim();
    const msg = validateState(incoming, props.states);
    if (msg != '') {
      setError(msg);
    } else {
      props.setStates([
        ...props.states,
        { name: incoming, type: type, invFunc: null },
      ]);
      setError('');
      props.toggle();
      props.toggleSnack(`State: "${incoming}" was added`);
      resetValues();
    }
  };

  const deleteState = () => {
    const incoming = state.trim();
    if (props.states.find((s) => s.name === incoming)) {
      props.setStates(props.states.filter((s) => s.name !== incoming));
      setError('');
      props.toggle();
      props.toggleSnack(`State: "${incoming}" was deleted`);
      resetValues();
    } else {
      setError(`State "${incoming}" does not currently exist`);
    }
  };
  return (
    <GenericForm
      title={'Add/Remove State'}
      isOpen={props.isOpen}
      onClose={() => {
        setError('');
        props.toggle();
      }}
      onSubmit={addState}
      submitText="Add State"
      closeText="Remove State"
      onCancel={deleteState}
    >
      <DialogContent>
        <DialogContentText>
          Please enter the name of the state that you wish to Add/Remove.
        </DialogContentText>
        <TextField
          autoFocus
          error={error !== ''}
          margin="dense"
          id="name"
          label="State Name"
          type="normal"
          fullWidth
          variant="standard"
          helperText={error}
          onChange={(e) => setState(e.target.value)}
        />
        <FormControl>
          <RadioGroup
            row
            aria-labelledby="demo-radio-buttons-group-label"
            defaultValue="none"
            name="radio-buttons-group"
          >
            <FormControlLabel
              value="start"
              onChange={() => setType('start')}
              control={
                <Radio sx={{ '& .MuiSvgIcon-root': { fontSize: 20 } }} />
              }
              label="Start"
            />
            <FormControlLabel
              value="final"
              onChange={() => setType('final')}
              control={
                <Radio sx={{ '& .MuiSvgIcon-root': { fontSize: 20 } }} />
              }
              label="Final"
            />
            <FormControlLabel
              value="startFinal"
              onChange={() => setType('startFinal')}
              control={
                <Radio sx={{ '& .MuiSvgIcon-root': { fontSize: 20 } }} />
              }
              label="Start & Final"
            />
            <FormControlLabel
              value="none"
              onChange={() => setType('normal')}
              control={
                <Radio sx={{ '& .MuiSvgIcon-root': { fontSize: 20 } }} />
              }
              label="Normal"
            />
            {isTmType(props.machineType) && (
              <FormControlLabel
                value="accept"
                onChange={() => setType('accept')}
                control={
                  <Radio sx={{ '& .MuiSvgIcon-root': { fontSize: 20 } }} />
                }
                label="Accept"
              />
            )}
          </RadioGroup>
        </FormControl>
      </DialogContent>
    </GenericForm>
  );
}
